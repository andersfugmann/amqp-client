open Thread
open Amqp_client_lib
open Spec.Connection

let version = "2.2.2"

let string_until c str =
  try
    let index = String.index str c in
    String.sub str 0 index
  with
  | Not_found -> str

type t = { framing: Framing.t;
           virtual_host: string;
           mutable channel: int;
           mutable closing: bool;
           mutable closed: unit Ivar.t option;
         }

let reply_start framing (username, password) =
  let print_item table s =
    let open Types in
    match List.assoc s table with
    | VLongstr v -> Log.info "%s: %s" s v
    | _ -> ()
    | exception _ -> ()
  in

  let reply { Start.version_major;
              version_minor;
              server_properties;
              mechanisms = _;
              locales } =

    let open Types in
    ["product"; "version" ] |> List.iter (print_item server_properties);
    Log.info "Amqp: %d.%d" version_major version_minor;

    return {
      Start_ok.mechanism = "PLAIN";
      response = "\x00" ^ username ^ "\x00" ^ password;
      locale = string_until ';' locales;
      Start_ok.client_properties = [
        "platform", VLongstr (Sys.os_type);
        "library", VLongstr "amqp-client (ocaml)";
        "version", VLongstr version;
        "client id", VLongstr (Framing.id framing);
        "capabilities", VTable [
          "publisher_confirms", VBoolean true;
          "exchange_exchange_bindings", VBoolean true;
          "basic.nack", VBoolean true;
          "consumer_cancel_notify", VBoolean true;
          "connection.blocked", VBoolean true;
          "consumer_priorities", VBoolean true;
          "authentication_failure_close", VBoolean true;
          "per_consumer_qos", VBoolean true;
        ]
      ];
    }
  in
  Start.reply (framing, 0) reply


let reply_tune framing =
  let var = Ivar.create () in
  let reply { Tune.channel_max;
              frame_max; heartbeat; } =
    Log.debug "Channel max: %d" channel_max;
    Log.debug "Frame_max: %d" frame_max;
    Log.debug "Heartbeat: %d" heartbeat;
    Ivar.fill var (if heartbeat = 0 then `Disabled else `Heartbeat heartbeat);
    Framing.set_max_length framing frame_max;
    return {
      Tune_ok.channel_max;
      frame_max;
      heartbeat;
    }
  in
  Tune.reply (framing, 0) reply >>= fun () ->
  Ivar.read var >>= fun v ->
  return v

let reply_close _t framing =
  let reply { Close.reply_code;
              reply_text;
              class_id = _;
              method_id = _;
            } =
    Log.info "Closed code: %d" reply_code;
    Log.info "Closed text: %s" reply_text;
    (* Ivar.fill t.closed (); *)
    return ()
  in
  Close.reply (framing, 0) reply

let rec send_heartbeat delay t =
  after (float delay *. 1000.0) >>= fun () ->
  if t.closing then
    return ()
  else begin
    Framing.send_heartbeat t.framing >>= fun () ->
    send_heartbeat delay t
  end

let register_blocked_handler framing =
  let (_, read_blocked) = Blocked.Internal.read in
  let (_, read_unblocked) = Unblocked.Internal.read in
  let blocked_handler { Blocked.reason } =
    Log.info "Connection blocked: %s" reason;
    Framing.set_flow_all framing true
  in
  let unblocked_handler () =
    Framing.set_flow_all framing false
  in
  read_blocked ~once:false blocked_handler (framing, 0);
  read_unblocked ~once:false unblocked_handler (framing, 0)

let open_connection { framing; virtual_host; _ } =
  Open.request (framing, 0) { Open.virtual_host } >>= fun x ->
  return x

let connection_closed t _s =
  match t with
  | { closed = Some ivar; _ } when Ivar.is_full ivar ->
    return ()
  | { closed = Some ivar; _ } ->
    Ivar.fill ivar ();
    Framing.close t.framing
  | { closing = false; _ } ->
    raise Types.Connection_closed
  | { closing = true; _ } ->
    return ()

let on_closed t =
  let ivar = match t.closed with
    | None ->
      let ivar = Ivar.create () in
      t.closed <- Some ivar;
      ivar
    | Some ivar ->
      ivar
  in
  Ivar.read ivar

let connect ~id ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) ?heartbeat host =

  let tcp_error_handler = ref (fun exn -> raise exn) in

  Tcp.connect ~exn_handler:(fun exn -> !tcp_error_handler exn) ~nodelay:() host port >>= fun (input, output) ->

  let framing = Framing.init ~id input output in
  let t =
    { framing; virtual_host; channel = 0; closing = false;
      closed = None }
  in
  let exn_handler exn = connection_closed t (Printexc.to_string exn) in
  tcp_error_handler := exn_handler;
  Framing.start framing (connection_closed t) >>= fun () ->
  spawn ~exn_handler (reply_close t framing);
  reply_start framing credentials >>= fun () ->
  reply_tune framing >>= fun server_heartbeat ->
  begin
    match heartbeat, server_heartbeat with
    | None, `Disabled -> ()
    | Some hb, `Disabled
    | None, `Heartbeat hb ->
      spawn ~exn_handler (send_heartbeat hb t);
    | Some hb, `Heartbeat hb' ->
      spawn ~exn_handler (send_heartbeat (min hb hb') t);
  end;
  open_connection t >>= fun () ->
  register_blocked_handler framing;
  return t

let connect_uri ~id uri =
  let u = Uri.of_string uri in
  let () = match Uri.scheme u with
    | None -> raise (Invalid_argument "scheme required")
    | Some "amqp" -> ()
    | Some scheme -> raise (Invalid_argument ("Unsupported scheme: " ^ scheme))
  in
  let credentials = match Uri.user u, Uri.password u with
    | Some user, Some password -> Some (user, password)
    | None, None -> None
    | _ -> failwith "Both user and password must be supplied"
  in

  let virtual_host = match Uri.path u with
    | "" -> None
    | vhost -> Some vhost
  in
  let heartbeat =
    match List.assoc "heartbeat_interval" (Uri.query u) with
    | [interval] ->
      Some (int_of_string interval)
    | _ ->
      raise (Invalid_argument "heartbeat_interval specified multiple times")
    | exception Not_found -> None
  in

  let host = match Uri.host u with
    | None -> raise (Invalid_argument "Uri must contain a host part")
    | Some h -> h
  in
  connect ~id ?virtual_host ?port:(Uri.port u) ?credentials ?heartbeat host

let open_channel ~id confirms t =
  t.channel <- t.channel + 1;
  Channel.create ~id confirms t.framing t.channel

let close t =
  t.closing <- true;
  Framing.flush t.framing >>= fun () ->
  Close.request (t.framing, 0) { Close.reply_code = 200;
                                 reply_text = "Closed on user request";
                                 class_id = 0;
                                 method_id = 0;
                               } >>= fun () ->
  Framing.close t.framing

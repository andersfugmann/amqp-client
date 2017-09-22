open Thread
open Spec.Connection

let version = "1.1.3"

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
        "library", VLongstr "ocaml-amqp";
        "version", VLongstr version;
        "client id", VLongstr (Framing.id framing);
        "capabilities", VTable [
          "publisher_confirms", VBoolean true;
          "exchange_exchange_bindings", VBoolean true;
          "basic.nack", VBoolean true;
          "consumer_cancel_notify", VBoolean false;
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

let reply_close framing =
  let reply { Close.reply_code;
              reply_text;
              class_id;
              method_id;
            } =
    Log.info "Reply code: %d" reply_code;
    Log.info "Reply test: %s" reply_text;
    Log.info "message_id: (%d, %d)" class_id method_id;
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
  match t.closing with
  | true ->
      Log.info "Close Received ok";
      return ()
  | false -> raise Types.Connection_closed

let connect ~id ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) ?heartbeat host =

  Tcp.connect ~nodelay:() host port >>= fun (input, output) ->

  let framing = Framing.init ~id input output in
  let t = { framing; virtual_host; channel = 0; closing = false } in
  Framing.start framing (connection_closed t) >>= fun () ->
  reply_start framing credentials >>= fun () ->
  reply_tune framing >>= fun server_heartbeat ->
  begin
    match heartbeat, server_heartbeat with
    | None, `Disabled -> ()
    | Some hb, `Disabled
    | None, `Heartbeat hb ->
      spawn (send_heartbeat hb t);
    | Some hb, `Heartbeat hb' ->
      spawn (send_heartbeat (min hb hb') t);
  end;
  open_connection t >>= fun () ->
  register_blocked_handler framing;
  spawn (reply_close framing);
  return t

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

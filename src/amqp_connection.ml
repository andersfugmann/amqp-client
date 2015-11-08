open Async.Std
open Amqp_spec.Connection

let log = Amqp_io.log

type t = { framing: Amqp_framing.t;
           virtual_host: string;
           mutable channel: int
         }

let rec string_split ?(offset=0) ~by s =
  match String.index_from s offset by with
  | n ->
    String.sub s offset (n - offset) :: string_split ~offset:(n+1) ~by s
  | exception Not_found -> [ String.sub s offset (String.length s - offset) ]
  | exception Invalid_argument _ -> []

let handle_start id (username, password) {Start.version_major;
                                       version_minor;
                                       server_properties;
                                       mechanisms = _;
                                       locales } =
  let open Amqp_types in
  let print_item table s =
    match List.assoc s table with
    | VLongstr v -> printf "%s: %s\n" s v
    | _ -> ()
    | exception _ -> ()
  in
  ["product"; "version" ] |> List.iter (print_item server_properties);
  printf "Amqp: %d.%d\n" version_major version_minor;

  let properties =
      [
        "platform", VLongstr (Sys.os_type);
        "library", VLongstr "ocaml-amqp";
        "version", VLongstr "0.0.1";
        "client id", VLongstr id;
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
      ]
  in

  return {
    Start_ok.client_properties = properties;
    mechanism = "PLAIN";
    response = "\x00" ^ username ^ "\x00" ^ password;
    locale = string_split ~by:';' locales |> List.hd
  }

let handle_tune framing { Tune.channel_max;
                  frame_max; heartbeat; } =
  log "Channel max: %d" channel_max;
  log "Frame_max: %d" frame_max;
  log "Heartbeat: %d" heartbeat;
  log "Send tune_ok";
  Amqp_framing.set_max_length framing frame_max;
  return {
    Tune_ok.channel_max;
    frame_max;
    heartbeat;
  }


let handle_close { Close.reply_code;
                   reply_text;
                   class_id;
                   method_id;
                 } =
  log "Reply code: %d" reply_code;
  log "Reply test: %s" reply_text;
  log "message_id: (%d, %d)" class_id method_id;
  return ()

let register_blocked_handler framing =

  let (_, read_blocked) = Blocked.Internal.read in
  let (_, read_unblocked) = Unblocked.Internal.read in
  let blocked_handler { Blocked.reason } =
    log "Connection blocked: %s" reason;
    Amqp_framing.set_flow_all framing true
  in
  let unblocked_handler () =
    Amqp_framing.set_flow_all framing false
  in
  read_blocked ~once:false blocked_handler (framing, 0);
  read_unblocked ~once:false unblocked_handler (framing, 0)

let open_connection { framing; virtual_host; _ } =
  Open.request (framing, 0) { Open.virtual_host }

let connect ~id ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) host =
  Amqp_framing.init ~id ~port host >>= fun framing ->
  let t = { framing; virtual_host; channel = 0 } in

  Start.reply (framing, 0) (handle_start (Amqp_framing.id framing) credentials) >>= fun () ->
  Tune.reply (framing, 0) (handle_tune framing) >>= fun () ->
  open_connection t >>= fun () ->
  register_blocked_handler t.framing;
  don't_wait_for (Close.reply (framing, 0) handle_close);
  return t

let open_channel ~id confirms t =
  t.channel <- t.channel + 1;
  Amqp_channel.create ~id confirms t.framing t.channel

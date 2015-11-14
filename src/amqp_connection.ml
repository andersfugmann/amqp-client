open Core.Std
open Async.Std
open Amqp_spec.Connection

let version = "0.1.1"
let log = Amqp_io.log

type t = { framing: Amqp_framing.t;
           virtual_host: string;
           mutable channel: int;
           closed: unit Ivar.t;
         }

let reply_start framing (username, password) =
  let print_item table s =
    let open Amqp_types in
    match List.Assoc.find table s with
    | Some (VLongstr v) -> printf "%s: %s\n" s v
    | _ -> ()
  in

  let reply { Start.version_major;
              version_minor;
              server_properties;
              mechanisms = _;
              locales } =

    let open Amqp_types in
    ["product"; "version" ] |> List.iter ~f:(print_item server_properties);
    printf "Amqp: %d.%d\n" version_major version_minor;

    return {
      Start_ok.mechanism = "PLAIN";
      response = "\x00" ^ username ^ "\x00" ^ password;
      locale = String.split ~on:';' locales |> List.hd_exn;
      Start_ok.client_properties = [
        "platform", VLongstr (Sys.os_type);
        "library", VLongstr "ocaml-amqp";
        "version", VLongstr version;
        "client id", VLongstr (Amqp_framing.id framing);
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
  let reply { Tune.channel_max;
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
  in
  Tune.reply (framing, 0) reply

let reply_close framing =
  let reply { Close.reply_code;
              reply_text;
              class_id;
              method_id;
            } =
    log "Reply code: %d" reply_code;
    log "Reply test: %s" reply_text;
    log "message_id: (%d, %d)" class_id method_id;
    return ()
  in
  Close.reply (framing, 0) reply

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

  let addr = Tcp.to_host_and_port host port in
  let closed = Ivar.create () in
  Tcp.connect addr >>= fun (socket, input, output) ->
  don't_wait_for (Reader.close_finished input >>| fun () -> Ivar.fill closed ());

  Socket.setopt socket Socket.Opt.nodelay true;

  Amqp_framing.init ~id input output >>= fun framing ->
  let t = { framing; virtual_host; channel = 0; closed } in

  reply_start framing credentials >>= fun () ->
  reply_tune framing >>= fun () ->
  open_connection t >>= fun () ->
  register_blocked_handler framing;
  don't_wait_for (reply_close framing);
  return t

let open_channel ~id confirms t =
  t.channel <- t.channel + 1;
  Amqp_channel.create ~id confirms t.framing t.channel

let close t =
  Amqp_framing.flush t.framing >>= fun () ->
  Close.request (t.framing, 0) { Close.reply_code = 200;
                                 reply_text = "Closed on user request";
                                 class_id = 0;
                                 method_id = 0;
                               } >>= fun () ->
  Amqp_framing.close t.framing

let closed t =
  Ivar.read t.closed

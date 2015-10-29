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
                                       server_properties = _;
                                       mechanisms;
                                       locales } =
  let open Amqp_types in
  log "Server version: %d %d" version_major version_minor;
  log "Mechanisms: %s" mechanisms;

  let properties =
      [
        "platform", VLongstr (Sys.os_type);
        "library", VLongstr "ocaml-amqp";
        "version", VLongstr "0.0.1";
        "client id", VLongstr id;
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


let handle_open_ok () =
  log "Open_ok";
  return ()

let handle_close { Close.reply_code;
                   reply_text;
                   class_id;
                   method_id;
                 } =
  log "Reply code: %d" reply_code;
  log "Reply test: %s" reply_text;
  log "message_id: (%d, %d)" class_id method_id;
  return ()

let open_connection { framing; virtual_host; _ } =
  Open.request (framing, 0) { Open.virtual_host } >>= handle_open_ok

let connect ~id ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) host =
  Amqp_framing.init ~id ~port host >>= fun framing ->
  let t = { framing; virtual_host; channel = 0 } in

  (* Ok. Lets try to start the thing. *)
  Start.reply (framing, 0) (handle_start (Amqp_framing.id framing) credentials) >>= fun () ->
  Tune.reply (framing, 0) (handle_tune framing) >>= fun () ->
  open_connection t >>= fun () ->

  (* We cannot wait for connection close *)
  don't_wait_for (Close.reply (framing, 0) handle_close);
  return t

let open_channel ~id ?confirms t =
  t.channel <- t.channel + 1;
  Amqp_channel.create ~id ?confirms t.framing t.channel

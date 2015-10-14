open Async.Std
open Batteries

let log fmt = printf (fmt ^^ "\n%!")

type t = { framing: Framing.t;
           channel: Channel.t;
           virtual_host: string;
         }

let handle_channel_open_ok () =
  log "Open_ok";
  return ()


let handle_start (username, password) {Spec.Connection.Start.version_major;
                  version_minor;
                  server_properties;
                  mechanisms;
                  locales } =
  log "Connection start";
  log "Id: %d %d" version_major version_minor;
  log "Properties: %s(%d)" (dump server_properties) (List.length server_properties);
  log "Mechanisms: %s" mechanisms;
  log "Locales: %s" locales;
  log "Send start_ok";
  return {
    Spec.Connection.Start_ok.client_properties = server_properties;
    mechanism = "PLAIN";
    response = "\x00" ^ username ^ "\x00" ^ password;
    locale = String.nsplit ~by:";" locales |> List.hd
  }

let handle_tune { Spec.Connection.Tune.channel_max;
                  frame_max; heartbeat; } =
  log "Channel max: %d" channel_max;
  log "Frame_max: %d" frame_max;
  log "Heartbeat: %d" heartbeat;
  log "Send tune_ok";
  return {
    Spec.Connection.Tune_ok.channel_max;
    frame_max;
    heartbeat;
  }


let handle_open_ok () =
  log "Open_ok";
  return ()

let handle_close { Spec.Connection.Close.reply_code;
                   reply_text;
                   class_id;
                   method_id;
                 } =
  log "Reply code: %d" reply_code;
  log "Reply test: %s" reply_text;
  log "message_id: (%d, %d)" class_id method_id;
  return ()

let open_connection { channel; virtual_host; _ } =
  let open Spec.Connection.Open in
  request channel { virtual_host } >>= handle_open_ok


let connect ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) ~host () =
  let (reader, writer) = Pipe.create () in
  Framing.init ~port ~host writer >>= fun framing ->
  let channel = Channel.init framing reader 0 in
  let t = { framing; channel; virtual_host } in

  (* Ok. Lets try to start the thing. *)
  Spec.Connection.Start.reply channel (handle_start credentials) >>= fun () ->
  Spec.Connection.Tune.reply channel handle_tune >>= fun () ->
  open_connection t >>= fun () ->
  (* We cannot wait for connection close *)
  don't_wait_for (Spec.Connection.Close.reply channel handle_close);

  return t

let open_channel { framing; _} n =
  let (reader, writer) = Pipe.create () in
  Framing.register_channel framing n writer;
  let c = Channel.init framing reader n in
  Spec.Channel.Open.request c () >>=
  handle_channel_open_ok >>= fun () ->
  return c

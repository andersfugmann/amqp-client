open Async.Std

let log = Amqp_protocol.log

type t = { framing: Amqp_framing.t;
           channel: Amqp_channel.t;
           virtual_host: string;
         }

let handle_channel_open_ok () =
  log "Open_ok";
  return ()

let rec string_split ?(offset=0) ~by s =
  match String.index_from s offset by with
  | n ->
    String.sub s offset (n - offset) :: string_split ~offset:(n+1) ~by s
  | exception Not_found -> [ String.sub s offset (String.length s - offset) ]
  | exception Invalid_argument _ -> []

let handle_start (username, password) {Amqp_spec.Connection.Start.version_major;
                  version_minor;
                  server_properties;
                  mechanisms;
                  locales } =
  log "Connection start";
  log "Id: %d %d" version_major version_minor;
  log "Properties: (%d)" (List.length server_properties);
  log "Mechanisms: %s" mechanisms;
  log "Locales: %s" locales;
  log "Send start_ok";
  return {
    Amqp_spec.Connection.Start_ok.client_properties = server_properties;
    mechanism = "PLAIN";
    response = "\x00" ^ username ^ "\x00" ^ password;
    locale = string_split ~by:';' locales |> List.hd
  }

let handle_tune { Amqp_spec.Connection.Tune.channel_max;
                  frame_max; heartbeat; } =
  log "Channel max: %d" channel_max;
  log "Frame_max: %d" frame_max;
  log "Heartbeat: %d" heartbeat;
  log "Send tune_ok";
  return {
    Amqp_spec.Connection.Tune_ok.channel_max;
    frame_max;
    heartbeat;
  }


let handle_open_ok () =
  log "Open_ok";
  return ()

let handle_close { Amqp_spec.Connection.Close.reply_code;
                   reply_text;
                   class_id;
                   method_id;
                 } =
  log "Reply code: %d" reply_code;
  log "Reply test: %s" reply_text;
  log "message_id: (%d, %d)" class_id method_id;
  return ()

let open_connection { channel; virtual_host; _ } =
  let open Amqp_spec.Connection.Open in
  request channel { virtual_host } >>= handle_open_ok


let connect ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) ~host () =
  let (reader, writer) = Pipe.create () in
  Amqp_framing.init ~port ~host writer >>= fun framing ->
  let channel = Amqp_channel.init framing reader 0 in
  let t = { framing; channel; virtual_host } in

  (* Ok. Lets try to start the thing. *)
  Amqp_spec.Connection.Start.reply channel (handle_start credentials) >>= fun () ->
  Amqp_spec.Connection.Tune.reply channel handle_tune >>= fun () ->
  open_connection t >>= fun () ->
  (* We cannot wait for connection close *)
  don't_wait_for (Amqp_spec.Connection.Close.reply channel handle_close);

  return t

let open_channel { framing; _} n =
  let (reader, writer) = Pipe.create () in
  Amqp_framing.register_channel framing n writer;
  let c = Amqp_channel.init framing reader n in
  Amqp_spec.Channel.Open.request c () >>=
  handle_channel_open_ok >>= fun () ->
  return c

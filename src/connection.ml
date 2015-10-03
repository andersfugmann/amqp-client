open Batteries

let log fmt = Printf.fprintf stdout (fmt ^^ "\n%!")

type t = { framing: Framing.t;
          channel: Channel.t }

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
  {
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
  {
    Spec.Connection.Tune_ok.channel_max;
    frame_max;
    heartbeat;
  }

let connect ?(port=5672) ?(credentials=("guest", "guest")) ~host () =
  let transport = Transport.connect ~port ~host in
  let framing = Framing.init transport in
  let channel = Channel.init framing 0 in

  Spec.Connection.Start.reply channel (handle_start credentials);
  Spec.Connection.Tune.reply channel handle_tune;
  { framing; channel }

let rec start t =
  (* Keep receiving messages *)
  Framing.read t.framing;
  start t

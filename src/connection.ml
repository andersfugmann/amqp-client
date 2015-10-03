open Batteries

let log fmt = Printf.fprintf stdout (fmt ^^ "\n%!")

type t = { framing: Framing.t;
           channel: Channel.t;
           virtual_host: string;
         }

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


let handle_open_ok { Spec.Connection.Open_ok.reserved_1 } =
  log "Open_ok";
  log "Reserved_1: %s" reserved_1;
  ()



let open_connection { channel; virtual_host; _ } () =
  let open Spec.Connection.Open in
  request channel { virtual_host;
                    reserved_1 = "";
                    reserved_2 = false } handle_open_ok

let connect ?(virtual_host="/") ?(port=5672) ?(credentials=("guest", "guest")) ~host () =
  let transport = Transport.connect ~port ~host in
  let framing = Framing.init transport in
  let channel = Channel.init framing 0 in

  let t = { framing; channel; virtual_host } in
  Spec.Connection.Start.reply channel (handle_start credentials);
  Spec.Connection.Tune.reply ~after:(open_connection t) channel handle_tune;
  t

let rec start t =
  (* Keep receiving messages *)
  Framing.read t.framing;
  start t

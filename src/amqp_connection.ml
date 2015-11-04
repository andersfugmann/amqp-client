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

let rec print_type indent t =
  let open Amqp_types in
  match t with
  | VTable t ->
    let indent' = indent ^ "  " in
    printf "[\n";
    List.iter (fun (k, v) -> printf "%s%s: " indent' k; print_type (indent')  v; printf "\n") t;
    printf "%s]" indent;
  | VBoolean v -> printf "%b" v
  | VShortshort v
  | VShort v
  | VLong v
  | VTimestamp v
  | VLonglong v -> printf "%d" v
  | VShortstr v
  | VLongstr v -> printf "%s" v
  | VFloat v
  | VDouble v-> printf "%f" v
  | VDecimal v -> printf "%f" (float v.value /. float v.digits)
  | VArray a ->
    let indent' = indent ^ "  " in
    printf "[\n";
    List.iter (fun v -> printf "%s" indent'; print_type (indent')  v; printf "\n") a;
    printf "%s]" indent;
  | VUnit _ -> printf "\n"

let handle_start id (username, password) {Start.version_major;
                                       version_minor;
                                       server_properties;
                                       mechanisms = _;
                                       locales } =
  let open Amqp_types in
  printf "Server properties:\n";
  print_type "" (VTable server_properties);
  printf "\nServer version: %d.%d\n" version_major version_minor;

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
          "connection.blocked", VBoolean false;
          "consumer_priorities", VBoolean false;
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

let open_connection { framing; virtual_host; _ } =
  Open.request (framing, 0) { Open.virtual_host }

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

let open_channel ~id confirms t =
  t.channel <- t.channel + 1;
  Amqp_channel.create ~id confirms t.framing t.channel

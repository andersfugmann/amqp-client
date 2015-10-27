open Async.Std
module Connection = Amqp_connection
module Channel = Amqp_channel

open Amqp_spec.Exchange

type t = { name : string }


(** Predefined Default exchange *)
let default    = { name="" }

(** Predefined Direct exchange *)
let amq_direct = { name = "amq.direct" }

(** Predefined Fanout exchange *)
let amq_fanout = { name = "amq.fanout" }

(** Predefined topic exchange *)
let amq_topic  = { name = "amq.topic" }

(** Predefined header exchange *)
let amq_header = { name = "amq.header" }

type exchange_type = Direct | Fanout | Topic | Headers

let string_of_exchange_type = function
  | Direct -> "direct"
  | Fanout -> "fanout"
  | Topic -> "topic"
  | Headers -> "headers"

let declare ?(passive=false) ?(durable=false) ?(auto_delete=false) ?(internal=false) channel ~exchange_type name =
  Declare.request (Channel.channel channel)
    { Declare.exchange = name;
      amqp_type = (string_of_exchange_type exchange_type);
      passive;
      durable;
      auto_delete;
      internal;
      no_wait = false;
      arguments = [] } >>= fun () ->
  return { name }

let delete ?(if_unused=false) channel t =
  Delete.request (Channel.channel channel)
    { Delete.exchange = t.name;
      if_unused;
      no_wait = false;
    }

let bind channel t ~routing_key source =
  Bind.request (Channel.channel channel)
    { Bind.destination = t.name;
      source = source.name;
      routing_key;
      no_wait = false;
      arguments = [];
    }

let unbind channel t ~routing_key source =
  Unbind.request (Channel.channel channel)
    { Unbind.destination = t.name;
      source = source.name;
      routing_key;
      no_wait = false;
      arguments = [];
    }

let publish channel t
    ?content_type
    ?content_encoding
    ?correlation_id
    ?message_id
    ?(mandatory=false)
    ?reply_to
    ?expiration
    ?(persistent=false)
    ?app_id
    ?headers
    ~routing_key
    data =
  let open Amqp_spec.Basic in
  let app_id = match app_id with
    | Some "" -> None
    | Some n -> Some n
    | None -> Some (Amqp_channel.id channel)
  in
  let delivery_mode = if persistent then Some 2 else None in
  Publish.request (Amqp_channel.channel channel)
    ({Publish.exchange = t.name;
      routing_key=routing_key;
      mandatory;
      immediate=false},
     (Content.init ?content_type
        ?content_encoding
        ?correlation_id
        ?reply_to
        ?message_id
        ?delivery_mode
        ?app_id
        ?headers
        ?expiration ()), data)

let name t = t.name

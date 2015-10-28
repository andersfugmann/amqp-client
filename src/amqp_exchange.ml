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

let declare ?(passive=false) ?(durable=false) ?(auto_delete=false) ?(internal=false) channel exchange_type name =
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
    ?(mandatory=false)
    ~routing_key
    (header, body) =

  let open Amqp_spec.Basic in
  let header = match header.Content.app_id with
    | Some _ -> header
    | None -> { header with Content.app_id = Some (Amqp_channel.id channel) }
  in
  Publish.request (Amqp_channel.channel channel)
    ({Publish.exchange = t.name;
      routing_key=routing_key;
      mandatory;
      immediate=false},
     header, body)
let name t = t.name

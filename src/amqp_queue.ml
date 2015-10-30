open Async.Std
module Connection = Amqp_connection
module Channel = Amqp_channel
module Exchange = Amqp_exchange
module Message = Amqp_message
open Amqp_spec.Queue

type t = { name: string }

let message_ttl v = "x-message-ttl", Amqp_types.VLonglong v
let auto_expire v = "x-expires", Amqp_types.VLonglong v
let max_length v = "x-max-length", Amqp_types.VLonglong v
let max_length_bytes v = "x-max-length-bytes", Amqp_types.VLonglong v
let dead_letter_exchange v = "x-dead-letter-exchange", Amqp_types.VLongstr v
let dead_letter_routing_key v = "x-dead-letter-routing-key", Amqp_types.VLongstr v
let maximum_priority v = "x-max-priotity", Amqp_types.VLonglong v

let declare channel ?(durable=false) ?(exclusive=false) ?(auto_delete=false) ?(arguments=[]) name =
  let channel = Amqp_channel.channel channel in
  let req = { Declare.queue=name; passive=false; durable; exclusive;
              auto_delete; no_wait=false; arguments }
  in
  Declare.request channel req >>= fun rep ->
  assert (rep.Declare_ok.queue = name);
  return { name }

let get ~no_ack channel t =
  let open Amqp_spec.Basic in
  let channel = Channel.channel channel in
  Get.request channel { Get.queue=t.name; no_ack } >>= function
  | `Get_empty () ->
    return None
  | `Get_ok (get_ok, (header, body))  ->
    return (Some { Message.delivery_tag = get_ok.Get_ok.delivery_tag;
                   Message.redelivered = get_ok.Get_ok.redelivered;
                   Message.exchange = get_ok.Get_ok.exchange;
                   Message.routing_key = get_ok.Get_ok.routing_key;
                   Message.message = (header, body) })

(** Publish a message directly to a queue *)
let publish channel t ?mandatory message =
  Exchange.publish channel Exchange.default ?mandatory
    ~routing_key:t.name
    message

type consumer = { channel: Amqp_framing.t * int; tag: string;
                  writer: Amqp_message.t Pipe.Writer.t }

(** Consume message from a queue. *)
let consume ~id ?(no_local=false) ?(no_ack=false) ?(exclusive=false) channel t =
  let open Amqp_spec.Basic in
  let (reader, writer) = Pipe.create () in
  let consumer_tag = Printf.sprintf "%s.%s" (Channel.Internal.unique_id channel) id in

  let to_writer (deliver, header, body) =
    { Message.delivery_tag = deliver.Deliver.delivery_tag;
      Message.redelivered = deliver.Deliver.redelivered;
      Message.exchange = deliver.Deliver.exchange;
      Message.routing_key = deliver.Deliver.routing_key;
      Message.message = (header, body) }
    |> Pipe.write_without_pushback writer
  in
  let on_receive channel consume_ok =
    Channel.Internal.register_consumer_handler channel consume_ok.Consume_ok.consumer_tag to_writer
  in
  let req = { Consume.queue=t.name;
              consumer_tag;
              no_local;
              no_ack;
              exclusive;
              no_wait = false;
              arguments = [];
            }
  in

  Consume.request ~post_handler:(on_receive channel) (Channel.channel channel) req >>= fun rep ->
  let tag = rep.Consume_ok.consumer_tag in
  return ({ channel = Channel.channel channel; tag; writer }, reader)

let cancel consumer =
  let open Amqp_spec.Basic in
  Cancel.request (consumer.channel) { Cancel.consumer_tag = consumer.tag; no_wait = false } >>= fun _rep ->
  Pipe.close consumer.writer;
  return ()

(** Bind a queue to an exhange.
    Messages posted on the exchange which match the routing key
    will be routed to the queue
*)
let bind channel t ~routing_key exchange =
  Bind.request (Channel.channel channel)
    { Bind.queue = t.name;
      exchange = (Exchange.name exchange);
      routing_key;
      no_wait = false;
      arguments = []
    }

let unbind channel t ~routing_key exchange =
  Unbind.request (Channel.channel channel)
    { Unbind.queue = t.name;
      exchange = (Exchange.name exchange);
      routing_key;
      arguments = []
    }

(** Purge the queue *)
let purge channel t =
  Purge.request (Channel.channel channel)
    { Purge.queue = t.name;
      no_wait = false;
    } >>= fun _rep ->
  return ()

(** Delete the queue. *)
let delete ?(if_unused=false) ?(if_empty=false) channel t =
  Delete.request (Channel.channel channel)
    { Delete.queue = t.name;
      if_unused;
      if_empty;
      no_wait = false;
    } >>= fun _rep -> return ()


(** Name of the queue *)
let name t = t.name

(** Construct a queue without any validation *)
let fake _channel name = return { name }

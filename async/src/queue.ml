open Thread
open Amqp_client_lib
open Spec.Queue

type t = { name: string }

let message_ttl v = "x-message-ttl", Types.VLonglong v
let auto_expire v = "x-expires", Types.VLonglong v
let max_length v = "x-max-length", Types.VLonglong v
let max_length_bytes v = "x-max-length-bytes", Types.VLonglong v
let dead_letter_exchange v = "x-dead-letter-exchange", Types.VLongstr v
let dead_letter_routing_key v = "x-dead-letter-routing-key", Types.VLongstr v
let maximum_priority v = "x-max-priority", Types.VLonglong v

let declare channel ?(durable=false) ?(exclusive=false) ?(auto_delete=false) ?(passive=false) ?(arguments=[]) ?(autogenerate=false) name =
  if autogenerate && String.length name != 0 then
    invalid_arg "Queue.declare name must be empty if autogenerate is true.";
  if not autogenerate && String.length name == 0 then
    invalid_arg "Queue.declare autogenerate must be true if name is empty.";
  let channel = Channel.channel channel in
  let req = { Declare.queue=name; passive; durable; exclusive;
              auto_delete; no_wait=false; arguments }
  in
  Declare.request channel req >>= fun rep ->
  if not autogenerate && name <> rep.Declare_ok.queue then
    failwith "Queue name returned by server doesn't match requested.";
  return { name = rep.Declare_ok.queue }

let get ~no_ack channel t =
  let open Spec.Basic in
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

type 'a consumer = { channel: 'a Channel.t;
                     tag: string;
                     writer: Message.t Pipe.Writer.t }

(** Consume message from a queue. *)
let consume ~id ?(no_local=false) ?(no_ack=false) ?(exclusive=false)
    ?on_cancel channel t =
  let open Spec.Basic in
  let (reader, writer) = Pipe.create () in
  let consumer_tag = Printf.sprintf "%s.%s" (Channel.Internal.unique_id channel) id
  in
  let on_cancel () =
    Pipe.close_without_pushback writer;
    match on_cancel with
    | Some f -> f ()
    | None -> raise (Types.Consumer_cancelled consumer_tag)
  in

  let to_writer (deliver, header, body) =
    { Message.delivery_tag = deliver.Deliver.delivery_tag;
      Message.redelivered = deliver.Deliver.redelivered;
      Message.exchange = deliver.Deliver.exchange;
      Message.routing_key = deliver.Deliver.routing_key;
      Message.message = (header, body) }
    |> Pipe.write_without_pushback writer
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
  let var = Ivar.create () in
  let on_receive consume_ok =
    Channel.Internal.register_consumer_handler channel consume_ok.Consume_ok.consumer_tag to_writer on_cancel;
    Ivar.fill var consume_ok
  in
  let read = snd Consume_ok.Internal.read in
  read ~once:true on_receive (Channel.channel channel);

  Consume.Internal.write (Channel.channel channel) req >>= fun () ->
  Ivar.read var >>= fun rep ->
  let tag = rep.Consume_ok.consumer_tag in
  return ({ channel; tag; writer }, reader)

let cancel consumer =
  let open Spec.Basic in
  Cancel.request (Channel.channel consumer.channel) { Cancel.consumer_tag = consumer.tag; no_wait = false } >>= fun _rep ->
  Channel.Internal.deregister_consumer_handler consumer.channel consumer.tag;
  Pipe.close consumer.writer

let bind channel t exchange = Exchange.Internal.bind_queue channel exchange t.name
let unbind channel t exchange = Exchange.Internal.unbind_queue channel exchange t.name

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

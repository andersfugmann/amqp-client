open Async.Std
let log = Amqp_protocol.log
type t = { queue: string }


let message_ttl v = "x-message-ttl", Amqp_types.VLonglong v
let auto_expire v = "x-expires", Amqp_types.VLonglong v
let max_length v = "x-max-length", Amqp_types.VLonglong v
let max_length_bytes v = "x-max-length-bytes", Amqp_types.VLonglong v
let dead_letter_exchange v = "x-dead-letter-exchange", Amqp_types.VLongstr v
let dead_letter_routing_key v = "x-dead-letter-routing-key", Amqp_types.VLongstr v
let maximum_priority v = "x-max-priotity", Amqp_types.VLonglong v



let declare channel ?(durable=false) ?(exclusive=false) ?(auto_delete=false) ?(arguments=[]) queue =
  let channel = Amqp_channel.channel channel in
  let req = { Amqp_spec.Queue.Declare.queue; passive=false; durable; exclusive;
              auto_delete; no_wait=false; arguments }
  in
  Amqp_spec.Queue.Declare.request channel req >>= fun rep ->
  assert (rep.Amqp_spec.Queue.Declare_ok.queue = queue);
  return { queue }


let get ~no_ack channel { queue } handler =
  let open Amqp_spec.Basic in
  let channel = Amqp_channel.channel channel in
  Get.request channel { Get.queue; no_ack } >>= function
  | `Get_empty () ->
    log "No Data"; return ()
  | `Get_ok (mth, (hdr, data))  ->
    handler mth hdr data >>= fun () ->
    if no_ack = false then
      Ack.request channel { Ack.delivery_tag = mth.Get_ok.delivery_tag; multiple = false }
    else
      return ()

let publish channel { queue }
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
    data =
  let open Amqp_spec.Basic in
  let app_id = match app_id with
    | Some "" -> None
    | Some n -> Some n
    | None -> Some (Amqp_channel.id channel)
  in
  let delivery_mode = if persistent then Some 2 else None in
  Publish.request (Amqp_channel.channel channel)
    ({Publish.exchange = ""; routing_key=queue; mandatory; immediate=false},
     (Content.init ?content_type
        ?content_encoding
        ?correlation_id
        ?reply_to
        ?message_id
        ?delivery_mode
        ?app_id
        ?headers
        ?expiration ()), data)

(* How do we handle ack. *)
let consume ?(no_local=false) ?(no_ack=false) ?(exclusive=false) channel { queue } handler =
  let open Amqp_spec.Basic in
  let (reader, writer) = Pipe.create () in

  let rec handle_messages channel reader handler =
    Pipe.read reader >>= function
    | `Eof -> return ()
    | `Ok (mth, (header, body)) ->
      handler mth header body >>= fun () ->
      let ack =
        if no_ack = false then
          Ack.request channel { Ack.delivery_tag = mth.Deliver.delivery_tag; multiple = false }
        else
          return ()
      in
      ack >>= fun () ->
      handle_messages channel reader handler
  in

  let on_receive channel consume_ok =
    Amqp_channel.register_consumer_handler channel consume_ok.Consume_ok.consumer_tag
      (Pipe.write_without_pushback writer)
  in
  let req = { Consume.queue;
              consumer_tag = ""; (* TODO: Do not autoselect for tracability *)
              no_local;
              no_ack;
              exclusive;
              no_wait = false;
              arguments = [] (* TODO: Understand rabbitmq arguments *);
            }
  in
  let cancel channel consumer_tag () =
    Cancel.request channel { Cancel.consumer_tag; no_wait = false } >>= fun _rep ->
    Pipe.close writer;
    return ()
  in


  Consume.request ~post_handler:(on_receive channel) (Amqp_channel.channel channel) req >>= fun rep ->
  (* Start message handling *)
  don't_wait_for (handle_messages (Amqp_channel.channel channel) reader handler);
  let consumer_tag = rep.Consume_ok.consumer_tag in
  return (cancel (Amqp_channel.channel channel) consumer_tag)

(* This will require an exchange *)
let bind _t _exchange = return ()
let unbind _t _exchange = return ()

let purge _t = return ()
let delete _t = return ()

let name { queue } = queue

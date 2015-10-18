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

let declare channel ?(passive=false) ?(durable=false) ?(exclusive=false) ?(auto_delete=false) ?(arguments=[]) queue =
  (* Wonder what the table is *)
  let req = { Amqp_spec.Queue.Declare.queue; passive; durable; exclusive;
              auto_delete; no_wait=false; arguments }
  in
  Amqp_spec.Queue.Declare.request channel req >>= fun rep ->
  assert (rep.Amqp_spec.Queue.Declare_ok.queue = queue);
  return { queue }
    (*(rep.Amqp_spec.Queue.Declare_ok.message_count,
          rep.Amqp_spec.Queue.Declare_ok.consumer_count)
    *)

let bind _ _  = ()
let unbind _ _ = ()

let get ~no_ack channel { queue } handler =
  let open Amqp_spec.Basic in
  Get.request channel { Get.queue; no_ack } >>= function
  | `Get_empty () ->
    log "No Data"; return ()
  | `Get_ok (mth, (hdr, data))  ->
    handler mth hdr data >>= fun () ->
    if no_ack = false then
      Ack.request channel { Ack.delivery_tag = mth.Get_ok.delivery_tag; multiple = false }
    else
      return ()


let publish channel { queue } data =
  let open Amqp_spec.Basic in
  Publish.request channel
    ({Publish.exchange = ""; routing_key=queue; mandatory=true; immediate=false},
     (Content.init ~content_type:"x-test-data" ()),
     data)

open Async.Std
type t = { name: string }

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
  return (rep.Amqp_spec.Queue.Declare_ok.message_count,
          rep.Amqp_spec.Queue.Declare_ok.consumer_count)

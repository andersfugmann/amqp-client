open Async.Std
type t = { name: string }

let declare channel ?(passive=false) ?(durable=false) ?(exclusive=false) ?(auto_delete=false) queue =
  (* Wonder what the table is *)
  let req = { Amqp_spec.Queue.Declare.queue; passive; durable; exclusive;
              auto_delete; no_wait=false; arguments = [] }
  in
  Amqp_spec.Queue.Declare.request channel req >>= fun rep ->
  return (rep.Amqp_spec.Queue.Declare_ok.message_count,
          rep.Amqp_spec.Queue.Declare_ok.consumer_count)

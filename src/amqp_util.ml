open Async.Std
open Amqp_types
open Amqp_protocol

let log fmt = printf (fmt ^^ "\n%!")

(*
Bits are pushed from the bottom - Oh, I need the bits when done
So if only the first is set, it must become the 16 bit. (I hate them!)
What if i convert it little endian???

*)
let request (message_type, message_id, spec, _make, apply) =
  let write = Spec.write spec in
  let request channel msg =
    let data =
      apply (write (Output.create ())) msg
    in
    Amqp_channel.write channel message_type message_id data
  in
  request

let request_content (message_type, message_id, spec, _make, apply) c_req =
  let write = Spec.write spec in
  let request channel (msg, content) =
    apply (write (Output.create ())) msg
    |> Amqp_channel.write channel message_type message_id;
    c_req channel content
  in
  request

let reply (message_type, message_id, spec, make, _apply) =
  let read = Spec.read spec in
  let var = Ivar.create () in
  let reply post_handler channel =
    let handler data =
      let req = read make data in
      Ivar.fill var req;
      Amqp_channel.remove_handler channel (message_type, message_id);
      post_handler channel
    in
    Amqp_channel.add_handler channel (message_type, message_id) handler;
    Ivar.read var
  in
  ((message_type, message_id), reply)

let reply_content spec (_, c_rep) =
  let tpe, reply = reply spec in
  let reply post_handler channel =
    reply post_handler channel >>= fun m ->
    c_rep ignore channel >>= fun c ->
    return (m, c)
  in
  (tpe, reply)

let request0 req = req

let reply0 (_, rep) = rep

let request1 req (_, rep) =
  fun channel msg ->
    req channel msg;
    rep ignore channel

let reply1 (_, rep) req =
  fun channel (handler : 'a -> 'b Deferred.t) ->
    rep ignore channel >>= handler >>= fun msg ->
    req channel msg;
    return ()

let request2 req (tpe1, rep1) id1 (tpe2, rep2) id2 =
  let unregister tpe channel =
    Amqp_channel.remove_handler channel tpe
  in
  fun channel msg ->
    req channel msg;
    (* Choose either one *)
    Deferred.any [
      rep1 (unregister tpe2) channel >>= (fun a -> return (id1 a));
      rep2 (unregister tpe1) channel >>= (fun a -> return (id2 a));
    ]

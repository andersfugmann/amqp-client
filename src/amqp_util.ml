open Async.Std
open Amqp_types
open Amqp_protocol

let request0 (message_type, message_id, spec, _make, apply) =
  let write = write spec in
  fun channel msg ->
    let data =
      apply (write (Output.create ())) msg
    in
    Amqp_channel.write channel message_type message_id data

let cancel channel (_message_type, message_id, _spec, _make, _apply) =
  Amqp_channel.remove_handler channel (Amqp_framing.Method, message_id)

let reply0 (message_type, message_id, spec, make, _apply) =
  let read = read spec in
  let var = Ivar.create () in
  fun post_handler channel ->
    let handler data =
      read make data |> Ivar.fill var;
      Amqp_channel.remove_handler channel (message_type, message_id);
      post_handler channel
    in
    Amqp_channel.add_handler channel (message_type, message_id) handler;
    Ivar.read var

let request1 req_spec rep_spec =
  let req = request0 req_spec in
  let rep = reply0 rep_spec in
  fun channel msg ->
    req channel msg;
    rep ignore channel

let reply1 req_spec rep_spec =
  let req = reply0 req_spec in
  let rep = request0 rep_spec in
  fun channel (handler : 'a -> 'b Deferred.t) ->
    req ignore channel >>= handler >>= fun msg ->
    rep channel msg;
    return ()


let request2 req_spec rep_spec1 id1 rep_spec2 id2 =
  let unregister (message_type, message_id, _, _, _) channel =
    Amqp_channel.remove_handler channel (message_type, message_id)
  in
  let req = request0 req_spec in
  let rep1 = reply0 rep_spec1 in
  let rep2 = reply0 rep_spec2 in
  fun channel msg ->
    req channel msg;
    (* Choose either one *)
    Deferred.any [
      rep1 (unregister rep_spec2) channel >>= (fun a -> return (id1 a));
      rep2 (unregister rep_spec1) channel >>= (fun a -> return (id2 a));
    ]

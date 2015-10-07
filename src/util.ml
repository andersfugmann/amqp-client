open Batteries
open Async.Std
open Types

let request0 (method_id, spec, _make, apply) =
  let write = write spec in
  fun channel msg ->
    let req =
      apply (write (IO.output_string ())) msg
      |> IO.close_out
    in
    Channel.send channel method_id req

let reply0 (method_id, spec, make, _apply) =
  let read = read spec in
  fun channel ->
    Channel.receive channel method_id |> Ivar.read >>= fun data ->
    let resp = read make (IO.input_string data) in
    return resp


let request1 req_spec rep_spec =
  let req = request0 req_spec in
  let rep = reply0 rep_spec in
  fun channel msg ->
    req channel msg;
    rep channel

let reply1 req_spec rep_spec =
  let req = reply0 req_spec in
  let rep = request0 rep_spec in
  fun channel (handler : 'a -> 'b Deferred.t) ->
    req channel >>= handler >>= fun msg ->
    rep channel msg;
    return ()

let request2 a b _ = request1 a b

let reply2 a b _ = reply1 a b

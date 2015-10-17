module P = Printf
open Async.Std
open Amqp

let log fmt = P.eprintf (fmt ^^ "\n%!")

let _ =
  let _ =
    Connection.connect ~host:"127.0.0.1" () >>= fun connection ->
    log "Connection started";
    Connection.open_channel connection 1 >>= fun channel ->
    log "Channel opened";
    let arguments =
      let open Queue in
      [ maximum_priority 7 ]
    in
    Queue.declare channel ~arguments "Anders" >>= fun queue ->
    Queue.get ~no_ack:true channel queue >>= fun () ->
    log "Test complete";
    return ()
  in
  Scheduler.go ()

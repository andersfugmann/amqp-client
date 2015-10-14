open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let _ =
  let _ =
    Connection.connect ~host:"127.0.0.1" () >>= fun connection ->
    log "Connection started";
    Connection.open_channel connection 1 >>= fun channel ->
    log "Channel opened";
    let arguments =
      let open Queue in
      [ message_ttl 2345678;
        dead_letter_exchange "amq.direct";
        maximum_priority 7 ]
    in
    Queue.declare channel ~arguments "Anders" >>= fun _ ->
    log "Test complete";
    return ()
  in
  Scheduler.go ()

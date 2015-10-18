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
      [ maximum_priority 7 ]
    in
    Queue.declare channel ~arguments "anders" >>= fun queue ->
    let rec loop i =
      Queue.publish channel queue (Printf.sprintf "Message: %d" i) >>= fun () ->
      Queue.get ~no_ack:false channel queue
        (fun msg -> log "Received Message: %s" msg; return ()) >>= fun () ->
      loop (i+1)
    in
    loop 1
  in
  Scheduler.go ()

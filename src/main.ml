open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let rec sync_loop channel queue i =
  Queue.publish channel queue (Printf.sprintf "Message: %d" i) >>= fun () ->
  Queue.get ~no_ack:false channel queue (fun _ _ msg -> log "Received: %s" msg; return ()) >>= fun () ->
  sync_loop channel queue (i+1)

let consume channel queue =
  let i = ref 0 in
  let handler _ _ data =
    log "Received: %s" data;
    incr i;
    Queue.publish channel queue (Printf.sprintf "Message: %d" !i)
  in
  Queue.consume channel queue handler


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
    Queue.publish channel queue "Message: 0" >>= fun () ->
    (* sync_loop channel queue 1 *)
    consume channel queue;
  in
  Scheduler.go ()

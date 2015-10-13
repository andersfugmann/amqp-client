open Async.Std
open Protocol

let _ =
  let _ =
    Connection.connect ~host:"127.0.0.1" () >>= fun connection ->
    log "Connection started";
    Connection.open_channel connection 1 >>= fun channel ->
    log "Channel opened";
    (* Queue.create channel "Anders" *)
    ignore channel;
    return ()
  in
  Scheduler.go ()

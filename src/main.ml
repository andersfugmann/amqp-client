open Async.Std
open Protocol

let _ =
  let _ =
    Connection.connect ~host:"127.0.0.1" () >>= fun connection ->
    log "Connection started";
    Connection.open_channel connection 1 >>= fun _channel ->
    log "Connection opened";
    ignore connection;
    return ()
  in
  Scheduler.go ()

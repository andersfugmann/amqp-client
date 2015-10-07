open Async.Std

let _ =
  let _ =
    Connection.connect ~host:"127.0.0.1" () >>= fun _t ->
    printf "Connection started\n";
    return ()
  in
  Scheduler.go ()

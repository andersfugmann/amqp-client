open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  Channel.close channel >>= fun () ->
  log "Channel closed";

  return ()

let _ =
  Scheduler.go ()

open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let handler s =
  log "Recieved request: %s" s;
  return s

let start =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Server.init channel queue handler >>= fun _server ->
  log "Listening for requsts";

  return ()

let _ =
  Scheduler.go ()

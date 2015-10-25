module P = Printf
open Async.Std
open Amqp

let log fmt =
  P.eprintf (fmt ^^ "\n%!")

let handler s =
  log "Recieved request: %s" s;
  return s

let start =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  let arguments = [Queue.dead_letter_exchange ""] in
  Queue.declare channel ~arguments "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Server.start channel queue handler >>= fun _server ->
  log "Listening for requsts";

  return ()

let _ =
  Scheduler.go ()

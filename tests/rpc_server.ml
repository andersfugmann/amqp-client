module Amqp = Amqp.Make(Amqp_thread_async)
open Amqp.Thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let handler (h, s) =
  log "Recieved request: %s" s;
  return (h, s)

let start =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel ~arguments:[Rpc.Server.queue_argument] "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Server.start channel queue handler >>= fun _server ->
  log "Listening for requsts";

  return ()

let _ =
  Scheduler.go ()

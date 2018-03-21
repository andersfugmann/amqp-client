open Async
open Amqp_client_async

let host = "localhost"

let run () =
  Amqp.Connection.connect ~id:"MyConnection" host >>= fun connection ->
  Amqp.Connection.open_channel ~id:"MyChannel" Amqp.Channel.no_confirm connection >>= fun channel ->
  Amqp.Queue.declare channel "MyQueue" >>= fun queue ->
  Amqp.Queue.publish channel queue (Amqp.Message.make "My Message Payload") >>= function `Ok ->
  Amqp.Channel.close channel >>= fun () ->
  Amqp.Connection.close connection >>= fun () ->
  Shutdown.shutdown 0; return ()

let _ =
  don't_wait_for (run ());
  Scheduler.go ()

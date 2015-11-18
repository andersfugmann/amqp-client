open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let test =
  Connection.connect ~id:"ocaml-amqp-tests" "localhost" >>= fun connection1 ->
  Connection.connect ~id:"ocaml-amqp-tests1" "localhost" >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  log "Connections closed";
  Shutdown.shutdown 0

let _ =
  Scheduler.go ()

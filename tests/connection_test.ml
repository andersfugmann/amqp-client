open Amqp_thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let test =
  Connection.connect ~id:"ocaml-amqp-tests" "localhost" >>= fun connection1 ->
  Connection.connect ~id:"ocaml-amqp-tests1" "localhost" >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  log "Connections closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

open Amqp
open Thread

let test =
  Connection.connect ~id:"ocaml-amqp-tests" "localhost" >>= fun connection1 ->
  Connection.connect ~id:"ocaml-amqp-tests1" "localhost" >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  Log.info "Connections closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

open Amqp_thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let test =
  Deferred.try_with (fun () -> Connection.connect ~id:"ocaml-amqp-tests" ~virtual_host:"/not_there" "localhost") >>= function
  | `Ok _ -> failwith "No exception raised"
  | `Error _ ->
      log "Got expected exception";
      Scheduler.shutdown 0;
      return ()

let _ =
    Scheduler.go ()

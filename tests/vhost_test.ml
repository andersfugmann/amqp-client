open Amqp
open Amqp_thread

let test =
  Deferred.try_with (fun () -> Connection.connect ~id:"ocaml-amqp-tests" ~virtual_host:"/not_there" "localhost") >>= function
  | `Ok _ -> failwith "No exception raised"
  | `Error _ ->
      Log.info "Got expected exception";
      Scheduler.shutdown 0;
      return ()

let _ =
    Scheduler.go ()

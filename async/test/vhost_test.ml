open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  Deferred.try_with (fun () -> Connection.connect ~id:(uniq "ocaml-amqp-tests") ~virtual_host:"/not_there" "localhost") >>= function
  | `Ok _ -> failwith "No exception raised"
  | `Error _ ->
      Log.info "Got expected exception";
      Scheduler.shutdown 0;
      return ()

let _ =
    Scheduler.go ()
let () = Printf.printf "Done\n"

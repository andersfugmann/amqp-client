open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Deferred.try_with (fun () -> Connection.connect ~id:(uniq "ocaml-amqp-tests") ~virtual_host:"/not_there" ?port "localhost") >>= function
  | `Ok _ -> failwith "No exception raised"
  | `Error _ ->
      Log.info "Got expected exception";
      Scheduler.shutdown 0;
      return ()

let _ =
    Scheduler.go ()
let () = Printf.printf "Done\n"

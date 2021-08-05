open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Thread.Deferred.try_with
    (fun () ->
       Connection.connect ~credentials:("invalid", "credentials") ~id:(uniq "ocaml-amqp-tests") ?port "localhost" >>= fun connection ->
       Connection.close connection
    ) >>| function
  | `Error Amqp_client_lib.Types.Connection_closed ->
    Scheduler.shutdown 0
  | _ ->
    Scheduler.shutdown 1

let _ =
  Scheduler.go ()

let () = Printf.printf "Done\n"

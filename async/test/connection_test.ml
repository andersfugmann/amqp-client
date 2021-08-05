open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ?port ~id:(uniq "ocaml-amqp-tests") "localhost" >>= fun connection1 ->
  Connection.connect ?port ~id:(uniq "ocaml-amqp-tests1") "localhost" >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  Log.info "Connections closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

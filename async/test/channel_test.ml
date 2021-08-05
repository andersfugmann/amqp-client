open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "ocaml-amqp-tests") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Deferred.List.init 600 ~f:(fun _ -> Connection.open_channel ~id:(uniq "test") Channel.no_confirm connection) >>= fun channels ->
  Log.info "Channels opened";
  Deferred.List.iter channels ~f:Channel.close >>= fun () ->
  Log.info "Channels closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

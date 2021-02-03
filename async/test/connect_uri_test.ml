open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" in
  let uri1, uri2 =
    let u1 = Printf.sprintf "amqp://localhost%s%s" in
    let u2 = Printf.sprintf "amqp://guest:guest@localhost:%s/?heartbeat_interval=11" in
    match port with
    | Some port ->
      u1 ":" port,
      u2 port
    | None ->
      u1 "" "",
      u2 "5672"
  in
  Connection.connect_uri ~id:(uniq "1") uri1 >>= fun connection1 ->
  Connection.connect_uri ~id:(uniq "2") uri2 >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  Log.info "Connections closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

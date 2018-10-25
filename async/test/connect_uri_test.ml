open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  Connection.connect_uri ~id:(uniq "1") "amqp://localhost" >>= fun connection1 ->
  Connection.connect_uri ~id:(uniq "2") "amqp://guest:guest@localhost:5672/?heartbeat_interval=11" >>= fun connection2 ->
  Connection.close connection1 >>= fun () ->
  Connection.close connection2 >>| fun () ->
  Log.info "Connections closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

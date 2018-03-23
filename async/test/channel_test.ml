open Amqp
open Thread
let test =
  Connection.connect ~id:"ocaml-amqp-tests" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Deferred.List.init 600 ~f:(fun _ -> Connection.open_channel ~id:"test" Channel.no_confirm connection) >>= fun channels ->
  Log.info "Channels opened";
  Deferred.List.iter channels ~f:Channel.close >>= fun () ->
  Log.info "Channels closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

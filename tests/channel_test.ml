module Amqp = Amqp.Make(Amqp_thread_async)
open Amqp.Thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let test =
  Connection.connect ~id:"ocaml-amqp-tests" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  Channel.close channel >>= fun () ->
  log "Channel closed";
  Deferred.List.init 600 ~f:(fun _ -> Connection.open_channel ~id:"test" Channel.no_confirm connection) >>= fun channels ->
  log "Channels opened";
  Deferred.List.iter channels ~f:Channel.close >>= fun () ->
  log "Channels closed";
  Connection.close connection >>| fun () ->
  log "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

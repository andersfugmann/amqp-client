open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")


let rec request t queue i =
  let req = Printf.sprintf "Echo: %d" i in
  Rpc.Client.call t ~ttl:10000 queue req >>= function
  | `Ok rep ->
    log "%s == %s" req rep;
    request t queue (i+1)
  | `Timedout ->
    log "Timedout";
    request t queue (i+1)


let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  (* Queue.declare channel "rpc.server.echo_reply" >>= fun queue -> *)
  Queue.fake channel "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Client.init ~id:"Test" connection >>= fun client ->
  request client queue 1

let _ =
  Scheduler.go ()

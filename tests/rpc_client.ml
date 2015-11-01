open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")


let rec request t i =
  let req = Printf.sprintf "Echo: %d" i in
  Rpc.Client.call ~ttl:1000 t Exchange.default ~routing_key:"rpc.server.echo_reply" (Message.make (string_of_int i)) >>= fun res ->
  begin
    match res with
    | Some (_, rep) -> log "%s == %s" req rep;
    | None -> log "%s: no reply" req;
  end;
  request t (i+1)

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  (*
  Connection.open_channel ~id:"rpc_test" Channel.no_confirm connection >>= fun channel ->
  Queue.declare channel ~arguments:[Rpc.Server.queue_argument] "rpc.server.echo_reply" >>= fun _queue ->
  *)
  Rpc.Client.init ~id:"Test" connection >>= fun client ->
  request client 1

let _ =
  Scheduler.go ()

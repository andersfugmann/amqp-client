open Amqp
open Amqp.Thread

let rec request t i =
  let req = Printf.sprintf "Echo: %d" i in
  Rpc.Client.call ~ttl:1000 t Exchange.default ~routing_key:"rpc.server.echo_reply" ~headers:[] (Message.make (string_of_int i)) >>= fun res ->
  begin
    match res with
    | Some (_, rep) -> Log.info "%s == %s" req rep;
    | None -> Log.info "%s: no reply" req;
  end;
  request t (i+1)

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.info "Connection started";
  (*
  Connection.open_channel ~id:"rpc_test" Channel.no_confirm connection >>= fun channel ->
  Queue.declare channel ~arguments:[Rpc.Server.queue_argument] "rpc.server.echo_reply" >>= fun _queue ->
  *)
  Rpc.Client.init ~id:"Test" connection >>= fun client ->
  request client 1

let _ =
  Scheduler.go ()

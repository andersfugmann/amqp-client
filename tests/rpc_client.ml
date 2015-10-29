open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")


let rec request t i =
  let req = Printf.sprintf "Echo: %d" i in
  Rpc.Client.call ~ttl:100 t Exchange.default ~routing_key:"rpc.server.echo_reply" (Message.make (string_of_int i)) >>= fun res ->
  begin
    match res with
    | Some (_, rep) -> log "%s == %s" req rep;
    | None -> log "No reply";
  end;
  request t (i+1)

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Rpc.Client.init ~id:"Test" connection >>= fun client ->
  request client 1

let _ =
  Scheduler.go ()

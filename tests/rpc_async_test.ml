open Amqp_thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let req_queue = "test.rpc"

let start_server channel =
  log "Server: starting";
  let handler (content, body) =
    let i = int_of_string body in
    after (Random.float 100.0) >>= fun () ->
    return (content, (string_of_int (i * i)))
  in
  log "Server: Declare queue";
  Queue.declare channel ~auto_delete:true req_queue >>= fun queue ->
  log "Server: Start server";
  Rpc.Server.start ~async:true channel queue handler >>= fun _ ->
  log "Server: ended";
  return ()

let call rpc_client i =
  after (Random.float 100.0) >>= fun () ->
  Rpc.Client.call ~ttl:100 rpc_client Exchange.default ~routing_key:req_queue ~headers:[] (Message.make (string_of_int i)) >>= function
  | Some (_, v) ->
    assert (int_of_string v = (i*i));
    return ()
  | None -> failwith "No reply"

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  spawn (start_server channel);
  log "Server started";
  Rpc.Client.init ~id:"rpc.client.test" connection >>= fun client ->
  log "Client initialized";
  Core.Std.List.init 1000 ~f:(call client) |> Deferred.all_unit >>= fun () ->
  log "All clients returned";
  Channel.close channel >>| fun () ->
  log "Channel closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

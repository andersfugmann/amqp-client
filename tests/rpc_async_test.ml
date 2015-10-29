open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let req_queue = "test.rpc"

let start_server channel =
  let handler (content, body) =
    let i = int_of_string body in
    after (Core.Std.Time.Span.of_ms (Random.float 100.0)) >>= fun () ->
    return (content, (string_of_int (i * i)))
  in
  Queue.declare channel ~auto_delete:true req_queue >>= fun queue ->
  Rpc.Server.start ~async:true channel queue handler >>= fun _ ->
  return ()

let call rpc_client queue i =
  after (Core.Std.Time.Span.of_ms (Random.float 100.0)) >>= fun () ->
  Rpc.Client.call ~ttl:100 rpc_client queue (Message.make (string_of_int i)) >>= function
  | Some (_, v) ->
    assert (int_of_string v = (i*i));
    return ()
  | None -> failwith "No reply"

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  don't_wait_for (start_server channel);
  Rpc.Client.init ~id:"rpc.client.test" connection >>= fun client ->
  Queue.fake channel req_queue >>= fun queue ->
  Core.Std.List.init 1000 ~f:(call client queue) |> Deferred.all_ignore >>= fun () ->
  log "All clients returned";
  Channel.close channel >>| fun () ->
  log "Channel closed";
  Shutdown.shutdown 0

let _ =
  Scheduler.go ()

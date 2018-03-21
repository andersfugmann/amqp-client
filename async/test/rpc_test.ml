open Amqp
open Amqp.Thread

let req_queue = "test.rpc"

let start_server channel =
  let handler (content, body) =
    let i = int_of_string body in
    return (content, (string_of_int (i * i)))
  in
  Queue.declare channel ~auto_delete:true req_queue >>= fun queue ->
  Rpc.Server.start channel queue handler >>= fun _ ->
  return ()

let rec run_tests rpc_client i =
  Rpc.Client.call ~ttl:100 rpc_client Exchange.default ~routing_key:req_queue ~headers:[] (Message.make (string_of_int i)) >>= function
  | Some (_, v) ->
    assert (int_of_string v = (i*i));
    if (i < 100) then run_tests rpc_client (i+1)
    else return ()
  | None -> failwith "No reply"

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  spawn (start_server channel);
  Rpc.Client.init ~id:"rpc.client.test" connection >>= fun client ->
  run_tests client 0 >>= fun () ->
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

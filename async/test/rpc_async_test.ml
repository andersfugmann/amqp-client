open Amqp
open Thread

let req_queue = "test.rpc"

let list_init ~f n =
  let rec inner = function
    | i when i = n -> []
    | i -> f i :: inner (i + 1)
  in
  inner 0

let start_server channel =
  let handler (content, body) =
    let i = int_of_string body in
    after (Random.float 100.0) >>= fun () ->
    return (content, (string_of_int (i * i)))
  in
  Queue.declare channel ~auto_delete:true req_queue >>= fun queue ->
  Rpc.Server.start ~async:true channel queue handler >>= fun _ ->
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
  Log.info "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  spawn (start_server channel);
  Log.info "Server started";
  Rpc.Client.init ~id:"rpc.client.test" connection >>= fun client ->
  Log.info "Client initialized";
  list_init 1000 ~f:(call client) |> Deferred.all_unit >>= fun () ->
  Log.info "All clients returned";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

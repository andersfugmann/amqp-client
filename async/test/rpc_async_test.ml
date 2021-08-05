open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let req_queue = (uniq "test.rpc")

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
  Rpc.Client.call ~ttl:1000 rpc_client Exchange.default ~routing_key:req_queue ~headers:[] (Message.make (string_of_int i)) >>= function
  | Some (_, v) ->
    assert (int_of_string v = (i*i));
    return ()
  | None -> failwith "No reply"

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  spawn (start_server channel);
  Log.info "Server started";
  Rpc.Client.init ~id:(uniq "rpc.client.test") connection >>= fun client ->
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

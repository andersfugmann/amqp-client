open Amqp
open Amqp_thread

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test () =
  Connection.connect ~virtual_host:"/" ~id:"ocaml-amqp-test" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"queue.test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:false "queue.test" >>= fun queue ->
  Log.info "Queue declared";
  Queue.publish channel queue (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  Log.info "Message published";
  Connection.close connection >>= fun () ->
  Log.info "Connection closed";
  Connection.connect ~virtual_host:"/" ~id:"ocaml-amqp-test" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"queue.test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:false "queue.test" >>= fun queue ->
  Log.info "Queue declared";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  (match m with
    | None -> failwith "No message"
    | Some _ -> ()
  );
  Log.info "Message received";
  Queue.delete channel queue >>= fun () ->
  Log.info "Queue deleted";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  test () |> ignore;
  Scheduler.go ()

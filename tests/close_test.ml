open Amqp_thread
open Amqp

let log fmt = Printf.printf (fmt ^^ "\n%!")

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test () =
  Connection.connect ~virtual_host:"/" ~id:"ocaml-amqp-test" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"queue.test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel ~auto_delete:false "queue.test" >>= fun queue ->
  log "Queue declared";
  Queue.publish channel queue (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  log "Message published";
  Connection.close connection >>= fun () ->
  log "Connection closed";
  Connection.connect ~virtual_host:"/" ~id:"ocaml-amqp-test" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"queue.test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel ~auto_delete:false "queue.test" >>= fun queue ->
  log "Queue declared";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  (match m with
    | None -> failwith "No message"
    | Some _ -> ()
  );
  log "Message received";
  Queue.delete channel queue >>= fun () ->
  log "Queue deleted";
  Channel.close channel >>= fun () ->
  log "Channel closed";
  Connection.close connection >>| fun () ->
  log "Connection closed";
  Scheduler.shutdown 0

let _ =
  test () |> ignore;
  Scheduler.go ()

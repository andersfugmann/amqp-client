open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel ~auto_delete:true "test" >>= fun queue ->
  log "Queue declared";
  Channel.set_prefetch channel ~count:100 >>= fun () ->
  log "Prefetch set";
  Queue.purge channel queue >>= fun () ->
  log "Queue purged";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  assert (m = None);
  log "Queue empty";
  Queue.publish channel queue (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  log "Message published";

  Queue.get ~no_ack:false channel queue >>= fun m ->
  let m = match m with
    | None -> failwith "No message"
    | Some m -> m
  in
  log "Message received";
  Message.ack channel m >>= fun () ->

  Exchange.declare channel Exchange.Topic "test_exchange" >>= fun exchange ->
  log "Exchange declared";
  Queue.bind channel queue exchange "test.#.key" >>= fun () ->
  log "Queue bind declared";

  Exchange.publish channel exchange ~routing_key:"test.a.b.c.key" (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  log "Message published";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  let m = match m with
    | None -> failwith "No message"
    | Some m -> m
  in
  log "Message recieved";
  Message.ack channel m >>= fun () ->
  Queue.delete channel queue >>| fun () ->
  log "Queue deleted";
  Shutdown.shutdown 0

let _ =
  Scheduler.go ()

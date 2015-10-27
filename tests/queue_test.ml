open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  log "Connection started";
  Connection.open_channel ~id:"test" connection >>= fun channel ->
  log "Channel opened";
  Queue.declare channel ~auto_delete:true "test" >>= fun queue ->
  log "Queue declared";
  Channel.set_prefetch channel ~count:100 >>= fun () ->
  log "Prefetch set";
  Queue.purge channel queue >>= fun () ->
  log "Queue purged";
  let var = Ivar.create () in
  Queue.get ~no_ack:false channel queue (handler var) >>= fun () ->
  assert (Ivar.is_empty var);
  log "Queue empty";

  Queue.publish channel queue "Test" >>= fun () ->
  log "Message published";
  Queue.get ~no_ack:false channel queue (handler var) >>= fun () ->
  assert (Ivar.is_full var);
  log "Message received";

  Exchange.declare channel ~exchange_type:Exchange.Topic "test_exchange" >>= fun exchange ->
  log "Exchange declared";
  Queue.bind channel queue exchange ~routing_key:"test.#.key" >>= fun () ->
  log "Queue bind declared";

  Exchange.publish channel exchange ~routing_key:"test.a.b.c.key" "Test" >>= fun () ->
  log "Message published";
  let var = Ivar.create () in
  Queue.get ~no_ack:false channel queue (handler var) >>= fun () ->
  assert (Ivar.is_full var);
  log "Message recieved";

  Queue.delete channel queue >>| fun () ->
  log "Queue deleted";
  Shutdown.shutdown 0

let _ =
  Scheduler.go ()

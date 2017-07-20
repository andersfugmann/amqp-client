open Amqp_thread
open Amqp

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"queue.test" Channel.with_confirm connection >>= fun channel ->
  Log.info "Channel opened";

  Exchange.publish channel Exchange.amq_direct ~mandatory:false ~routing_key:"non_existant_queue" (Message.make "") >>= fun r ->
  assert (r = `Ok);
  Exchange.publish channel Exchange.amq_direct ~mandatory:true ~routing_key:"non_existant_queue" (Message.make "") >>= fun r ->
  assert (r = `Failed);

  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

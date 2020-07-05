open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let received_count = ref 0

let handle_message channel consumer message =
  let f redeliver_count (message) =
    match redeliver_count with
    | 10 ->
      Log.debug "Received 10'th redelivery";
      Queue.cancel consumer >>= fun () ->
      assert (!received_count = 10);
      Message.ack channel message
    | _ ->
      incr received_count;
      Message.reject channel ~requeue:true message
  in
  Message.with_redeliver_count channel ~f message >>= fun _ ->
  return ()

let test =
  let routing_key = "test.redeliver1" in
  Connection.connect ~id:(uniq "") "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Channel.set_prefetch ~count:3 channel >>= fun () ->
  Queue.declare channel ~auto_delete:true "test.redeliver" >>= fun queue ->
  Queue.consume ~id:"test.redeliver" channel queue >>= fun (consumer, pipe) ->
  Queue.bind channel queue Exchange.amq_topic (`Topic routing_key) >>= fun () ->
  Exchange.publish channel Exchange.amq_topic ~routing_key (Message.make "redeliver me") >>= fun _ ->

  Pipe.iter pipe ~f:(fun message -> handle_message channel consumer message) >>= fun () ->
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

let () = Printf.printf "Done\n"

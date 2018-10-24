open Amqp_client_async
open Thread

(** Send an rpc request to queue a.
    We then read from queue a, post on queue b, read from b and then reply to
    the rpc request.

    We provide both a synchronous way to doing this and a parallel way,
    where queues are not consumed 'in order'.
*)

let handler_a queue_b channel message : unit Deferred.t =
  Queue.publish channel queue_b message.Message.message >>= fun `Ok ->
  return ()

(* Reply to the message *)
let handler_b channel message =
  let content, data = message.Message.message in
  let reply_text = "Echo: " ^ data in
  let reply_message =
    Message.make ?correlation_id:content.correlation_id reply_text
  in
  match content with
  | { Spec.Basic.Content.reply_to = Some reply_to; _ } ->
    Exchange.publish channel Exchange.default ~routing_key:reply_to reply_message >>= fun `Ok ->
    return ()
  | _ -> Printf.printf "No reply destination for message: %s" data;
    return ()

let consumer_cancelled () =
  Log.info "Consumer cancelled"

let _ =
  (* Setup queue a *)
  Connection.connect ~id:"multi_receive_example" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:true "multi_receive_example.a" >>= fun queue_a ->
  Queue.declare channel ~auto_delete:true "multi_receive_example.b" >>= fun queue_b ->

  (* Setup a pipe for consuming messages from queue a. We do not ack
     the messages and require exclusive access to the queue, i.e. no
     other consumers must be present for the queue *)

  Queue.consume ~id:"relay_a" ~no_ack:true ~exclusive:true channel queue_a >>= fun (_consumer_a, reader_a) ->
  Queue.consume ~id:"reply_b" ~on_cancel:consumer_cancelled ~no_ack:true ~exclusive:true channel queue_b >>= fun (_consumer_b, reader_b) ->


  spawn (Pipe.iter reader_a ~f:(handler_a queue_b channel));
  spawn (Pipe.iter reader_b ~f:(handler_b channel));

  (* Now lets start query *)
  Rpc.Client.init ~id:"req" connection >>= fun rpc ->
  let rec request i =
    let message = Message.make (string_of_int i) in
    Rpc.Client.call rpc ~headers:[] ~ttl:5000 ~routing_key:(Queue.name queue_a) Exchange.default message >>= function
    | Some (_, data) ->
      Printf.printf "Reply: %s\n%!" data;
      request (i + 1)
    | None ->
      request (i + 1)
  in
  request 0 >>= fun () ->
  return ()

let () =
  Scheduler.go ()

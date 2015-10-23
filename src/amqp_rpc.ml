open Async.Std
module Connection = Amqp_connection
module Channel = Amqp_channel
module Queue = Amqp_queue
open Amqp_spec.Basic

module Client = struct
  (* Each client should have its own channel *)
  (* We then register correlation-id and have callers wait for an Ivar.
     If we are running rabbitmq, we do not need to create a new queue, but can use amq.rabbit.reply-to
  *)
  type t = { queue: Queue.t;
             channel: Channel.t;
             id: string;
             outstanding: (string, [ `Ok of string | `Timedout ]  Ivar.t) Hashtbl.t;
             mutable counter: int;
           }

  (* We we receive the request with the redelivered flag set, then
     its an error. *)
  let handle_reply t ok content data =
    let reply = match ok with
      | true -> `Ok data
      | false -> `Timedout
    in
    match content.Content.correlation_id with
    | Some id ->
      begin match Hashtbl.find t.outstanding id with
        | var ->
          Ivar.fill var reply;
          Hashtbl.remove t.outstanding id;
          return ()
        | exception Not_found -> failwith ("Unknown correlation id: " ^ id)
      end
    | None -> failwith "No correlation id seems wrong"

  let init ~id connection =
    Connection.open_channel ~id:"rpc_client" connection >>= fun channel ->
    let id = Printf.sprintf "%s.%s" (Channel.id channel) id in
    Queue.declare channel ~exclusive:true ~auto_delete:true id >>= fun queue ->

    let t = { queue; channel; id; outstanding = Hashtbl.create 0; counter = 0 } in
    Queue.consume ~no_ack:true ~exclusive:true channel queue
      (fun deliver -> handle_reply t (not deliver.Deliver.redelivered)) >>= fun _stop ->
    Channel.on_return channel (fun (_, (h, d)) -> handle_reply t false h d);
    return t

  let call t ~ttl queue request =
    let correlation_id = Printf.sprintf "%s.%d" t.id t.counter in
    t.counter <- t.counter + 1;
    let reply_to = Queue.name t.queue in
    Queue.publish t.channel
      ~correlation_id ~expiration:ttl ~mandatory:true ~reply_to
      queue request

  (** Release resources *)
  let close t =
    Hashtbl.iter (fun _ var -> Ivar.fill var `Timedout) t.outstanding;
    Queue.delete t.queue >>= fun () ->
    Channel.close t.channel >>= fun () ->
    return ()
end

module Server = struct
  (* We need to define a queue, and listen receive on this queue *)
  type t = unit
  let init channel queue handler =
    ignore (channel, queue, handler);
    return ()

  let stop t = ignore t; return ()
end

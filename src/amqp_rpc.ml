open Async.Std
module Connection = Amqp_connection
module Channel = Amqp_channel
module Queue = Amqp_queue
module Exchange = Amqp_exchange
module Message = Amqp_message
open Amqp_spec.Basic

module Client = struct

  type t = { queue: Queue.t;
             channel: Channel.t;
             id: string;
             outstanding: (string, Message.message option Ivar.t) Hashtbl.t;
             mutable counter: int;
             consumer: Queue.consumer;
           }

  let handle_reply t ok (content, body) =
    let reply = match ok with
      | true -> Some (content, body)
      | false -> None
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
    | None -> failwith "No correlation id set"

  let init ~id connection =
    Connection.open_channel ~id:"rpc_client" connection >>= fun channel ->
    let id = Printf.sprintf "%s.%s" (Channel.id channel) id in
    Queue.declare channel
      ~exclusive:true
      ~auto_delete:true
      id >>= fun queue ->

    Queue.consume ~id:"rpc_client" ~no_ack:true ~exclusive:true channel queue >>= fun (consumer, reader) ->
    let t = { queue; channel; id; outstanding = Hashtbl.create 0; counter = 0; consumer } in
    don't_wait_for (Pipe.iter reader ~f:(fun { Message.message; _ } -> handle_reply t true message));
    Channel.on_return channel (fun (_, message) -> handle_reply t false message);

    return t

  let call t ~ttl ~routing_key exchange (header, body) =
    let correlation_id = Printf.sprintf "%s.%d" t.id t.counter in
    t.counter <- t.counter + 1;
    let reply_to = Some (Queue.name t.queue) in
    (* Register handler for the reply before sending the query *)
    let var = Ivar.create () in
    Hashtbl.add t.outstanding correlation_id var;
    let expiration = Some (string_of_int ttl) in
    let header = { header with Content.correlation_id = Some correlation_id;
                               expiration;
                               reply_to; }
    in
    Exchange.publish t.channel ~mandatory:true ~routing_key exchange (header, body) >>= fun () ->
    Ivar.read var


  (** Release resources *)
  let close t =
    Hashtbl.iter (fun _ var -> Ivar.fill var None) t.outstanding;
    Amqp_queue.cancel t.consumer >>= fun () ->
    Amqp_queue.delete t.channel t.queue >>= fun () ->
    Channel.close t.channel >>= fun () ->
    return ()
end

module Server = struct

  open Amqp_spec.Basic
  (* The server needs a queue name and a handler *)

  type t = { consumer: Queue.consumer }
  let start ?(async=false) channel queue handler =
    let handler ({ Message.message = (content, body); _ } as message) =

      let routing_key = match content.Content.reply_to with
        | Some r -> r
        | None -> failwith "Missing reply_to in reposnse"
      in

      let correlation_id = content.Content.correlation_id in

      handler (content, body) >>= fun (content, body) ->
      let content = { content with Content.correlation_id } in
      Exchange.publish channel Exchange.default
        ~routing_key (content, body) >>= fun () ->
      Message.ack channel message
    in
    (* Start consuming. *)
    Queue.consume ~id:"rpc_server" channel queue >>= fun (consumer, reader) ->
    let read = match async with
      | true -> (Pipe.iter_without_pushback reader ~f:(fun m -> don't_wait_for (handler m)))
      | false -> Pipe.iter reader ~f:handler
    in
    don't_wait_for read;
    return { consumer }

  let stop t =
    Queue.cancel t.consumer
end

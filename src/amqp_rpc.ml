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

    let t = { queue; channel; id; outstanding = Hashtbl.create 0; counter = 0 } in
    Queue.consume ~id:"rpc_client" ~no_ack:true ~exclusive:true channel queue
      (fun { Message.message; _ }-> handle_reply t true message) >>= fun _stop ->
    Channel.on_return channel (fun (_, message) -> handle_reply t false message);
    return t

  let call t ~ttl queue (header, body) =
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
    Queue.publish t.channel ~mandatory:true queue (header, body) >>= fun () ->
    Ivar.read var


  (** Release resources *)
  let close t =
    Hashtbl.iter (fun _ var -> Ivar.fill var None) t.outstanding;
    Amqp_queue.delete t.channel t.queue >>= fun () ->
    Channel.close t.channel >>= fun () ->
    return ()
end

module Server = struct

  open Amqp_spec.Basic
  (* The server needs a queue name and a handler *)

  type t = { consumer: Queue.consumer }
  let start channel queue handler =
    let handler { Message.message = (header, body); _ } =

      let routing_key = match header.Content.reply_to with
        | Some r -> r
        | None -> failwith "Missing reply_to in reposnse"
      in

      let correlation_id = header.Content.correlation_id in

      handler (header, body) >>= fun (header, body) ->
      let header = { header with Content.correlation_id } in
      Exchange.publish channel Exchange.default
        ~routing_key
        (header, body)
    in
    (* Start consuming *)
    Queue.consume ~id:"rpc_server" channel queue handler >>= fun consumer ->
    return { consumer }

  let stop t =
    Queue.cancel t.consumer
end

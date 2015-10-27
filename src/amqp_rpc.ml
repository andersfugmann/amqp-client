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
             outstanding: (string, [ `Ok of string | `Timedout ]  Ivar.t) Hashtbl.t;
             mutable counter: int;
           }

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
      (fun { Message.message = (hdr, body); _ }-> handle_reply t true hdr body) >>= fun _stop ->
    Channel.on_return channel (fun (_, (h, d)) -> handle_reply t false h d);
    return t

  let call t ~ttl queue request =
    let correlation_id = Printf.sprintf "%s.%d" t.id t.counter in
    t.counter <- t.counter + 1;
    let reply_to = Queue.name t.queue in
    (* Register handler for the reply before sending the query *)
    let var = Ivar.create () in
    Hashtbl.add t.outstanding correlation_id var;
    Queue.publish t.channel
      ~correlation_id ~expiration:(string_of_int ttl) ~mandatory:true ~reply_to
      queue request >>= fun () ->
    Ivar.read var


  (** Release resources *)
  let close t =
    Hashtbl.iter (fun _ var -> Ivar.fill var `Timedout) t.outstanding;
    Amqp_queue.delete t.channel t.queue >>= fun () ->
    Channel.close t.channel >>= fun () ->
    return ()
end

module Server = struct

  open Amqp_spec.Basic
  (* The server needs a queue name and a handler *)

  type t = { consumer: Queue.consumer }
  let start channel queue handler =
    let handler { Message.message = (content, body); _ } =

      let routing_key = match content.Content.reply_to with
        | Some r -> r
        | None -> failwith "Missing reply_to in reposnse"
      in

      let correlation_id = match content.Content.correlation_id with
        | Some r -> r
        | None -> failwith "Missing correnlation_id in reposnse"
      in
      handler body >>= fun reply ->
      Exchange.publish channel Exchange.default
        ~correlation_id
        ~routing_key
        reply
    in
    (* Start consuming *)
    Queue.consume ~id:"rpc_server" channel queue handler >>= fun consumer ->
    return { consumer }

  let stop t =
    Queue.cancel t.consumer
end

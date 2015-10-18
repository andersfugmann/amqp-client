open Async.Std
open Amqp_protocol

exception Busy

type method_handler = Input.t -> unit
type content_handler = Input.t * string -> unit

type t = { framing: Amqp_framing.t;
           input: Amqp_framing.message Pipe.Reader.t;
           channel_no: int;
           method_handlers: (Amqp_types.message_id, method_handler) Hashtbl.t;
           content_handlers: (Amqp_types.class_id, content_handler) Hashtbl.t;
         }

let write_content t class_id content data =
  log "Send content on channel: %d (%d)" t.channel_no class_id;
  Amqp_framing.write_content t.framing t.channel_no class_id content data

let write_method t message_id data =
  log "Send method on channel: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id);
  Amqp_framing.write_method t.framing t.channel_no message_id data

let print_handlers t =
  Hashtbl.iter (fun (cid, mid) _ -> log "Method Handler: (%d, %d)" cid mid) t.method_handlers;
  Hashtbl.iter (fun cid _ -> log "Content Handler: (%d)\n" cid) t.content_handlers

let read t =
  Pipe.read t.input >>= function
  | `Ok (Amqp_framing.Method (message_id, data)) ->
    log "Received method: (%d, %d) on channel %d. Data length: %d"
      (fst message_id) (snd message_id) t.channel_no (Input.length data);
    begin match Hashtbl.find t.method_handlers message_id with
      | handler ->
        handler data;
        print_handlers t;
        return t
      | exception Not_found ->
        failwith (Printf.sprintf "Unhandled method: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id))
    end
  | `Ok (Amqp_framing.Content (class_id, content, data)) ->
    log "Received content: %d on channel %d. Content: %d Data: %d"
      class_id t.channel_no (Input.length content) (String.length data);
    begin match Hashtbl.find t.content_handlers class_id with
      | handler ->
        handler (content, data);
        print_handlers t;
        return t
      | exception Not_found ->
        failwith (Printf.sprintf "Unhandled content: %d %d" t.channel_no class_id)
    end
  | `Eof -> failwith "Connection closed"

let flush t = Amqp_framing.flush t.framing

let add_method_handler t message_id handler =
  Hashtbl.add t.method_handlers message_id handler

let remove_method_handler t message_id =
  Hashtbl.remove t.method_handlers message_id

let add_content_handler t class_id handler =
  Hashtbl.add t.content_handlers class_id handler

let remove_content_handler t class_id =
  Hashtbl.remove t.content_handlers class_id

let init framing input channel_no =
  let method_handlers = Hashtbl.create 0 in
  let content_handlers = Hashtbl.create 0 in
  let t = { framing; input; channel_no; method_handlers; content_handlers } in
  Deferred.forever t read;
  t

let id { channel_no; _} = channel_no

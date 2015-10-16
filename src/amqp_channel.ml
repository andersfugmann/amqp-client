open Async.Std
open Amqp_protocol

exception Busy
exception Unhandled_message of Amqp_framing.message_type * Amqp_types.message_id

type handler = Input.t -> unit

type t = { framing: Amqp_framing.t;
           input: Amqp_framing.message Pipe.Reader.t;
           channel_no: int;
           handlers: ((Amqp_framing.message_type * Amqp_types.message_id), handler) Hashtbl.t;
         }


let write t message_type message_id data =
  log "Send method on channel: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id);
  Amqp_framing.write_message t.framing t.channel_no message_type message_id data

(* Reception is a continious repeat of reading messages and setting the correct ivar. *)
let read t =
  Pipe.read t.input >>= function
  | `Ok { Amqp_framing.message_type; message_id; data } ->
    log "Received method: (%d, %d) on channel %d"
      (fst message_id) (snd message_id) t.channel_no;
    begin match Hashtbl.find t.handlers (message_type, message_id) with
      | handler ->
        handler data;
        return t
      | exception Not_found ->
        failwith (Printf.sprintf "Unhandled message: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id))
    end
  | `Eof -> failwith "Connection closed"

let add_handler t (message_type, message_id) handler =
  Hashtbl.add t.handlers (message_type, message_id) handler

let remove_handler t (message_type, message_id) =
  Hashtbl.remove t.handlers (message_type, message_id)

let receive t (message_type, message_id) =
  let var = Ivar.create () in
  let handler data =
    remove_handler t (message_type, message_id);
    Ivar.fill var data;
  in
  add_handler t (message_type, message_id) handler;
  var


let init framing input channel_no =
  let handlers = Hashtbl.create 0 in
  let t = { framing; input; channel_no; handlers } in
  Deferred.forever t read;
  t

let id { channel_no; _} = channel_no

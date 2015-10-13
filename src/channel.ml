open Async.Std
open Protocol

exception Busy
exception Unhandled_message of Types.message_id

(* Callbacks are the reception of a string.
   We use ivar for async
*)
type t = { framing: Framing.t;
           input: Framing.message Pipe.Reader.t;
           channel_no: int;
           handlers: (Types.message_id, Input.t Ivar.t) Hashtbl.t;
         }



let write_method t message_id data =
  log "Send method on channel: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id);
  Framing.write_method_frame t.framing t.channel_no message_id data

(* Reception is a continious repeat of reading messages and setting the correct ivar. *)
let read t =
  Pipe.read t.input >>= function
  | `Ok { Framing.message_type = Framing.Method; message_id; data } ->
    log "Received method on channel : %d (%d, %d)"
      t.channel_no (fst message_id) (snd message_id);
    begin match BatHashtbl.find_option t.handlers message_id with
      | Some var -> Ivar.fill var data
      | None ->
        failwith (Printf.sprintf "Unhandled message: %d (%d, %d)" t.channel_no (fst message_id) (snd message_id))
    end;
    return t
  | `Ok { Framing.message_type = Framing.Body; _ } -> failwith "Cannot handle body message yet"
  | `Eof -> failwith "Connection closed"

let receive t message_id =
  if Hashtbl.mem t.handlers message_id then raise Busy;
  let var = Ivar.create () in
  Hashtbl.add t.handlers message_id var;
  var

let init framing input channel_no =
  let handlers = Hashtbl.create 0 in
  let t = { framing; input; channel_no; handlers } in
  Deferred.forever t read;
  t

let id { channel_no; _} = channel_no

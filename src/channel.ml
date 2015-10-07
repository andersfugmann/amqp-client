open Async.Std


exception Busy
exception Unhandled_message of Types.message_id

(* Callbacks are the reception of a string.
   We use ivar for async
*)
type t = { framing: Framing.t;
           input: (Types.message_id * Framing.message) Pipe.Reader.t;
           channel_no:int;
           handlers: (Types.message_id, string Ivar.t) Hashtbl.t;
         }

let send t method_id data = Framing.write_frame t.framing t.channel_no method_id (Framing.Method data)

(* Reception is a continious repeat of reading messages and setting the correct ivar. *)

let read t =
  Pipe.read t.input >>= function
  | `Ok (message_id, Framing.Method data) ->
    begin match BatHashtbl.find_option t.handlers message_id with
      | Some var -> Ivar.fill var data
      | None -> failwith "Unhandled message"
    end;
    return t
  | `Ok (_, Framing.Body _) -> failwith "Cannot handle body message yet"
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

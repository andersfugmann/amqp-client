open Batteries

exception Busy
exception Unhandled_message of Types.message_id

type callback = (string -> unit)
type t = { framing: Framing.t;
           channel_no:int;
           callbacks: (Types.message_id, callback) Hashtbl.t;
         }

let send t cid mid data = Framing.write t.framing t.channel_no (cid, mid) (Framing.Method data)
let send_body _t _cls _mth _data = ()
let receive t cid mid callback =
  if Hashtbl.mem t.callbacks (cid, mid) then raise Busy;
  Hashtbl.add t.callbacks (cid, mid) callback

let receive_body _t _cls _mth _callback = ()

let on_receive t message_id message =
  match message with
  | Framing.Method data ->
    begin
      match Hashtbl.find_option t.callbacks message_id with
      | Some callback ->
        callback data;
        Hashtbl.remove t.callbacks message_id
      | None -> raise (Unhandled_message message_id)
    end
  | Framing.Body _ -> failwith "Cannot handle body message"

let init framing channel_no =
  let callbacks = Hashtbl.create 0 in
  let t = { framing; channel_no; callbacks } in
  Framing.register_callback framing channel_no (on_receive t);
  t

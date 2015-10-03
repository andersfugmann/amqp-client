open Batteries
open Types

exception Unknown_frame_type of int

type channel_id = int

type channel_state =
  | Ready
  | Waiting of message_id * int * Buffer.t

type message =
  | Method of string
  | Body of string

type callback = message_id -> message -> unit

type channel = { callback: callback;
                 mutable state: channel_state;
               }

type t = { transport: Transport.t; channels: (channel_id, channel) Hashtbl.t }

let frame_end = 0xce

let frame          = Octet :: Short :: Longstr :: Octet :: Nil
let read_frame     = read frame
let write_frame    = write frame

let method_frame   = Short :: Short :: Nil
let content_header = Short :: Short :: Longlong :: Short :: Nil

let read_method_frame = read method_frame
let write_method_frame : (string IO.output -> 'a)  = write method_frame

let read_content_header = read content_header
let write_content_header  : (string IO.output -> 'a) = write content_header

let init transport =
  let channels = Hashtbl.create 0 in
  { transport; channels }

let write t channel (cid, mid) message =
  let (data, tpe) = match message with
    | Method data -> data, Constants.frame_method
    | Body data -> data, Constants.frame_header (* TODO *)
  in
  let hdr_data =
    write_method_frame (IO.output_string ()) cid mid
    |> IO.close_out
  in
  write_frame (t.transport.Transport.output) tpe channel (hdr_data ^ data) frame_end
  |> IO.flush

let read t =
  let input = t.transport.Transport.input in
  let (tpe, channel_id, data, magic) = read_frame (Tuple4.curry identity) input in
  assert (magic = frame_end);
  Printf.printf "Channel: %d\n%!" channel_id;
  let channel = Hashtbl.find t.channels channel_id in
  match tpe with
  | n when n = Constants.frame_method ->
    (* Standard method message *)
    assert (channel.state = Ready);
    let hdr = read_method_frame (Tuple2.curry identity) (IO.input_string data) in
    let data = String.slice ~first:4 data in
    channel.callback hdr (Method data)
  | n when n = Constants.frame_header ->
    assert (channel.state = Ready);
    (* Decode the header frame *)
    let cls_id, mth_id, size, _magic =
      read_content_header
        (Tuple4.curry identity)
        (IO.input_string data)
    in
    channel.state <- Waiting ((cls_id, mth_id), size, Buffer.create size)
  | n when n = Constants.frame_body ->
    begin
      match channel.state with
      | Ready -> failwith "Channel not expecting data frames"
      | Waiting (hdr, size, buffer) ->
        Buffer.add_string buffer data;
        if (size <= Buffer.length buffer) then begin
          channel.callback hdr (Body (Buffer.contents buffer));
          channel.state <- Ready
        end
    end
  | n when n = Constants.frame_heartbeat ->
    ()
  | n -> raise (Unknown_frame_type n)

let register_callback t channel callback =
  Hashtbl.add t.channels channel { callback; state = Ready }

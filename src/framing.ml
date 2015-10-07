open Async.Std
open Types

exception Unknown_frame_type of int

type channel_no = int

type channel_state =
  | Ready
  | Waiting of message_id * int * Buffer.t

type message =
  | Method of string
  | Body of string

type channel = { mutable state: channel_state;
                 writer: (message_id * message) Pipe.Writer.t;
               }

type t = { input: Reader.t; output: Writer.t;
           channels: (channel_no, channel) Hashtbl.t }

let frame_end = 0xce

let frame          = Octet :: Short :: Longstr :: Octet :: Nil
let read_frame     = read (Octet :: Short :: Long :: Nil)
let write_frame    = write frame
let protocol_header = "AMQP\x00\x00\x09\x01"
let method_frame = Short :: Short :: Nil
let read_method_frame = read method_frame
let content_header = Short :: Short :: Longlong :: Short :: Nil
let read_content_header = read content_header
let write_method_frame = write method_frame

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode t tpe channel_no msg =
  let open Batteries in
  let channel = Hashtbl.find t.channels channel_no in
  match tpe with
  | n when n = Constants.frame_method ->
    (* Standard method message *)
    assert (channel.state = Ready);
    let hdr = read_method_frame (Tuple.Tuple2.curry identity) (IO.input_string msg) in
    let msg = String.slice ~first:4 msg in
    Pipe.write_without_pushback channel.writer (hdr, Method msg);
  | n when n = Constants.frame_header ->
    assert (channel.state = Ready);
    (* Decode the header frame *)
    let cls_id, mth_id, size, _magic =
      read_content_header
        (Tuple4.curry identity)
        (IO.input_string msg)
    in
    channel.state <- Waiting ((cls_id, mth_id), size, Buffer.create size)
  | n when n = Constants.frame_body ->
    begin
      match channel.state with
      | Ready -> failwith "Channel not expecting data frames"
      | Waiting (hdr, size, buffer) ->
        Buffer.add_string buffer msg;
        if (size == Buffer.length buffer) then begin
          Pipe.write_without_pushback channel.writer (hdr, Body msg);
          channel.state <- Ready
        end
    end
  | n when n = Constants.frame_heartbeat ->
    failwith "Cannot handle heartbeat yet";
  | n -> raise (Unknown_frame_type n)

(* We should make read be using deferred. It would be so much easier *)
let read_frame t =
  (* Octet :: Short :: Longstr :: Octet *)
  let buf = Bytes.create (1+2+4) in
  Reader.really_read t.input buf >>=
  function `Eof _ -> failwith "Connection closed"
         | `Ok ->
           let (tpe, channel_no, data_length) =
             read_frame (fun a b c -> (a, b, c)) (BatIO.input_string buf)
           in
           printf "Read: %d %d %d\n" tpe channel_no data_length;
           (* read the message *)
           let msg = Bytes.create data_length in
           Reader.really_read t.input msg >>=
           function `Eof _ -> failwith "Connection closed"
                  | `Ok ->
                    Reader.read_char t.input >>=
                    function `Ok c when Char.code c = frame_end ->
                      decode t tpe channel_no msg;
                      return t
                           | `Ok c -> failwith (Printf.sprintf "Unexpected char : %x" (Char.code c))
                           | `Eof -> failwith "Connection closed"

let write_frame t channel_no (cid, mid) message =
  let open Batteries in
  let (data, tpe) = match message with
    | Method data -> data, Constants.frame_method
    | Body data -> data, Constants.frame_header (* TODO *)
  in
  let hdr_data =
    write_method_frame (IO.output_string ()) cid mid
    |> IO.close_out
  in
  write_frame (IO.output_string ()) tpe channel_no (hdr_data ^ data) frame_end
  |> IO.close_out
  |> Writer.write t.output

(** [writer] is channel 0 writer. It must be attached *)
let init ~port ~host writer =
  let addr = Tcp.to_host_and_port host port in
  Tcp.connect addr >>= fun (_socket, input, output) ->
      let channel = { state = Ready; writer } in
      let channels =
        let t = Hashtbl.create 0 in
        Hashtbl.add t 0 channel;
        t
      in

      let t = { input; output; channels } in
      Deferred.forever t read_frame;

      (* Send the connect string now *)
      Writer.write output protocol_header;
      return t

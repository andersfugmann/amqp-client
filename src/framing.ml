open Async.Std
open Types
open Protocol

exception Unknown_frame_type of int
exception Connection_closed

type channel_no = int

type channel_state =
  | Ready
  | Waiting of message_id * int * Buffer.t

type message_type =
  | Method
  | Body

type message = { message_type : message_type;
                 message_id : message_id;
                 data : Input.t;
               }

type data = Input.t

type channel = { mutable state: channel_state;
                 writer: message Pipe.Writer.t;
               }

type t = { input: Reader.t; output: Writer.t;
           channels: (channel_no, channel) Hashtbl.t }

let frame_end = '\xce'

let protocol_header = "AMQP\x00\x00\x09\x01"
let frame          = Octet :: Short :: Longstr :: Octet :: Nil
let read_method_frame = read (Short :: Short :: Nil)
let content_header = Short :: Short :: Longlong :: Short :: Nil
let read_content_header = read content_header

let log fmt = printf (fmt ^^ "\n%!")

(* Should register a monitor *)
let (>>) a b =
  a >>= function `Eof _ -> raise Connection_closed
               | `Ok -> b ()

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no data =
  let channel = Hashtbl.find t.channels channel_no in
  match tpe with
  | n when n = Constants.frame_method ->
    (* Standard method message *)
    assert (channel.state = Ready);
    let input = Input.create data in
    let message_id = read_method_frame (fun a b -> a, b) input in
    Pipe.write_without_pushback channel.writer
      { message_type = Method; message_id; data = input }
  | n when n = Constants.frame_header ->
    assert (channel.state = Ready);
    (* Decode the header frame *)
    let message_id, size, _flags =
      read_content_header (fun a b c d -> (a, b), c, d) (Input.create data)
    in
    channel.state <- Waiting (message_id, size, Buffer.create size)
  | n when n = Constants.frame_body ->
    begin
      match channel.state with
      | Ready -> failwith "Channel not expecting data frames"
      | Waiting (message_id, size, buffer) ->
        Buffer.add_string buffer data;
        if (size == Buffer.length buffer) then begin
          let data = Buffer.contents buffer in
          Pipe.write_without_pushback channel.writer
            { message_type = Body; message_id; data = Input.create data };
          channel.state <- Ready
        end
    end
  | n when n = Constants.frame_heartbeat ->
    failwith "Cannot handle heartbeat yet";
  | n -> raise (Unknown_frame_type n)

let read_frame t =
  (* Octet :: Short :: Longstr :: Octet *)
  let buf = Bytes.create (1+2+4) in
  Reader.really_read t.input buf >> fun () ->
  let input = Input.create buf in
  let tpe = decode Octet input in
  let channel_no = decode Short input in
  let length = decode Long input in
  log "Read: %d %d %d" tpe channel_no length;
  (* read the message *)
  let data = Bytes.create length in
  Reader.really_read t.input data >> fun () ->
  Reader.read_char t.input >>= function
  | `Ok c when c = frame_end ->
    decode_message t tpe channel_no data;
    return t
  | `Ok c -> failwith (Printf.sprintf "Unexpected char : %x" (Char.code c))
  | `Eof -> failwith "Connection closed"

let write t data =
  Writer.write ~len:(Output.length data) t.output (Output.buffer data)

let write_method_frame t channel_no (cid, mid) data =
  let output = Output.create ~size:(1+2+4+2+2) () in
  encode Octet output Constants.frame_method;
  encode Short output channel_no;
  encode Long  output (Output.length data + 4);
  encode Short output cid;
  encode Short output mid;
  write t output;
  write t data;
  Writer.write_char t.output frame_end

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

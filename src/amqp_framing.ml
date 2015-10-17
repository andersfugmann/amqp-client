open Async.Std
open Amqp_types
open Spec
open Amqp_protocol

exception Unknown_frame_type of int
exception Connection_closed

type channel_no = int

type channel_state =
  | Ready
  | Waiting of class_id * Input.t * int * Buffer.t

type message =
  | Method of message_id * Input.t
  | Content of class_id * Input.t * string

type data = Input.t

type channel = { mutable state: channel_state;
                 writer: message Pipe.Writer.t;
               }

type t = { input: Reader.t; output: Writer.t;
           channels: (channel_no, channel) Hashtbl.t;
           max_length: int;
         }

let frame_end = Char.chr (Amqp_constants.frame_end)

let protocol_header = "AMQP\x00\x00\x09\x01"
(* let frame          = Octet :: Short :: Longstr :: Octet :: Nil *)
let read_method_frame = read (Short :: Short :: Nil)
let read_content_header = read (Short :: Short :: Longlong :: Nil)

(* read_content_header = read content_header *)

(* Should register a monitor *)
let (>>) a b =
  a >>= function `Eof _ -> raise Connection_closed
               | `Ok -> b ()

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no data =
  let channel = Hashtbl.find t.channels channel_no in
  match tpe with
  | n when n = Amqp_constants.frame_method ->
    (* Standard method message *)
    assert (channel.state = Ready);
    let input = Input.create data in
    let message_id = read_method_frame (fun a b -> a, b) input in
    Pipe.write_without_pushback channel.writer
      (Method (message_id, input))
  | n when n = Amqp_constants.frame_header ->
    assert (channel.state = Ready);
    let input = Input.create data in
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in
    if size = 0 then
      (* Handle empty messages *)
      Pipe.write_without_pushback channel.writer
        (Content (class_id, input, ""))
    else
      channel.state <- Waiting (class_id, input, size, Buffer.create size)
  | n when n = Amqp_constants.frame_body ->
    begin
      match channel.state with
      | Ready -> failwith "Channel not expecting body frames"
      | Waiting (class_id, content, size, buffer) ->
        Buffer.add_string buffer data;
        if (size = Buffer.length buffer) then begin
          Pipe.write_without_pushback channel.writer
            (Content (class_id, content, Buffer.contents buffer));
          channel.state <- Ready
        end
    end
  | n when n = Amqp_constants.frame_heartbeat ->
    (* TODO: Send back a heartbeat frame *)
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
  (* read the message *)
  let data = Bytes.create length in
  Reader.really_read t.input data >> fun () ->
  Reader.read_char t.input >>= function
  | `Ok c when c = frame_end ->
    decode_message t tpe channel_no data;
    return t
  | `Ok c -> failwith (Printf.sprintf "Unexpected char : %x" (Char.code c))
  | `Eof -> failwith "Connection closed"

let write_frame t channel_no premable premable_length data =
  let output = Output.create ~size:(1+2+4+premable_length) () in
  encode Octet output Amqp_constants.frame_method;
  encode Short output channel_no;
  let sizer = Output.size_ref output in
  premable output;
  sizer (Output.length data);
  Writer.write ~len:(Output.length output) t.output (Output.buffer output);
  Writer.write ~len:(Output.length data) t.output (Output.buffer data);
  Writer.write_char t.output frame_end

let write_method t channel_no (cid, mid) data =
  let premable output =
    encode Short output cid;
    encode Short output mid;
  in
  write_frame t channel_no premable (2+2) data

let write_content t channel_no class_id content (data:string) =
  let data = Output.init data in
  let premable output =
    encode Short output class_id;
    encode Short output 0;
    encode Longlong output (Output.length data);
  in
  write_frame t channel_no premable (2+2+8) content;

  (* TODO: Allow interleaving to avoid starvation *)
  let rec send_data = function
    | offset when offset < (Output.length data) ->
      (* Send the next chunk *)
      let sub = Output.sub ~start:offset ~length:t.max_length data in
      write_frame t channel_no ignore 0 sub;
      send_data (offset + t.max_length)
    | _ -> ()
  in
  send_data 0


let register_channel { channels; _ } n writer =
  let channel = { state = Ready; writer } in
  Hashtbl.add channels n channel

(** [writer] is channel 0 writer. It must be attached *)
let init ~port ~host writer =
  let addr = Tcp.to_host_and_port host port in
  Tcp.connect addr >>= fun (_socket, input, output) ->
  let channels = Hashtbl.create 0 in
  let t = { input; output; channels; max_length = 256 } in
  register_channel t 0 writer;
  Deferred.forever t read_frame;

  Writer.write output protocol_header;
  return t

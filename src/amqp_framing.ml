open Async.Std
open Amqp_types
open Spec
open Amqp_protocol

exception Unknown_frame_type of int
exception Connection_closed
exception Busy
exception Unhandled_message

type channel_no = int

type channel_state =
  | Ready
  | Waiting of class_id * Input.t * int * Buffer.t

type message =
  | Method of message_id * Input.t
  | Content of class_id * Input.t * string

type data = Input.t

type content_handler = data * string -> unit
type method_handler = data -> unit

type channel = { mutable state: channel_state;
                 method_handlers: (message_id, method_handler) Hashtbl.t;
                 content_handlers: (class_id, content_handler) Hashtbl.t;
               }

type t = { input: Reader.t; output: Writer.t;
           channels: (channel_no, channel) Hashtbl.t;
           max_length: int;
         }

type channel_t =  t * channel_no

let frame_end = Char.chr (Amqp_constants.frame_end)

let protocol_header = "AMQP\x00\x00\x09\x01"
let read_method_frame = read (Short :: Short :: Nil)
let read_content_header = read (Short :: Short :: Longlong :: Nil)

(* Should register a monitor *)
let (>>) a b =
  a >>= function `Eof _ -> raise Connection_closed
               | `Ok -> b ()

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no data =
  let channel = Hashtbl.find t.channels channel_no in
  match channel.state, tpe with
  | Ready, n when n = Amqp_constants.frame_method ->
    (* Standard method message *)
    let input = Input.create data in
    let message_id = read_method_frame (fun a b -> a, b) input in
    log "Received method on channel: %d (%d, %d)" channel_no (fst message_id) (snd message_id);
    begin
      match Hashtbl.find channel.method_handlers message_id with
      | handler -> handler input
      | exception Not_found -> raise Unhandled_message
    end
  | Ready, n when n = Amqp_constants.frame_header ->
    let input = Input.create data in
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in
    log "Received header on channel: %d (%d). Size: %d" channel_no class_id size;

    if size = 0 then begin
      log "Received body on channel: %d (%d)" channel_no class_id;
      match Hashtbl.find channel.content_handlers class_id with
        | handler ->
          handler (input, "")
        | exception Not_found -> raise Unhandled_message
      end
    else
      channel.state <- Waiting (class_id, input, size, Buffer.create size)
  | Waiting (class_id, content, size, buffer), n when n = Amqp_constants.frame_body ->
    log "Received body data on channel: %d (%d) (S:%d, B:%d, D:%d)" channel_no class_id size (Buffer.length buffer) (String.length data);

    Buffer.add_string buffer data;
    if (size = Buffer.length buffer) then begin
      channel.state <- Ready;
      log "Received body on channel: %d (%d)" channel_no class_id;
      match Hashtbl.find channel.content_handlers class_id with
      | handler ->
        handler (content, Buffer.contents buffer);
      | exception Not_found -> raise Unhandled_message
    end
  | _, n when n = Amqp_constants.frame_heartbeat ->
    (* TODO: Send back a heartbeat frame *)
    failwith "Cannot handle heartbeat yet";
  | _, n -> raise (Unknown_frame_type n)

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

let write_frame t channel_no tpe premable premable_length data =
  let output = Output.create ~size:(1+2+4+premable_length) () in
  encode Octet output tpe;
  encode Short output channel_no;
  let sizer = Output.size_ref output in
  premable output;
  sizer (Output.length data);
  Writer.write ~len:(Output.length output) t.output (Output.buffer output);
  Writer.write ~len:(Output.length data) t.output (Output.buffer data);
  Writer.write_char t.output frame_end



let write_method (t, channel_no) (cid, mid) data =
  log "Send method on channel: %d (%d, %d)" channel_no cid mid;

  let premable output =
    encode Short output cid;
    encode Short output mid;
  in
  write_frame t channel_no Amqp_constants.frame_method premable (2+2) data

let write_content (t, channel_no) class_id content (data_s:string) =
  log "Send content on channel: %d (%d)" channel_no class_id;
  let data = Output.init ~offset:(String.length data_s) data_s in
  let premable output =
    encode Short output class_id;
    encode Short output 0;
    encode Longlong output (Output.length data);
  in
  write_frame t channel_no Amqp_constants.frame_header premable (2+2+8) content;

  (* TODO: Allow interleaving to avoid starvation *)
  let rec send_data = function
    | offset when offset < (Output.length data) ->
      (* Send the next chunk *)
      let sub = Output.sub ~start:offset ~length:t.max_length data in
      write_frame t channel_no Amqp_constants.frame_body ignore 0 sub;
      send_data (offset + t.max_length)
    | _ -> ()
  in
  send_data 0

let register_method_handler (t, channel_no) message_id handler =
  let c = Hashtbl.find t.channels channel_no in
  if Hashtbl.mem c.method_handlers message_id then raise Busy;
  Hashtbl.add c.method_handlers message_id handler

let register_content_handler (t, channel_no) class_id handler =
  let c = Hashtbl.find t.channels channel_no in
  if Hashtbl.mem c.content_handlers class_id then raise Busy;
  Hashtbl.add c.content_handlers class_id handler

let deregister_method_handler (t, channel_no) message_id =
  let c = Hashtbl.find t.channels channel_no in
  if not (Hashtbl.mem c.method_handlers message_id) then raise Busy;
  Hashtbl.remove c.method_handlers message_id

let deregister_content_handler (t, channel_no) class_id =
  let c = Hashtbl.find t.channels channel_no in
  if not (Hashtbl.mem c.content_handlers class_id) then raise Busy;
  Hashtbl.remove c.content_handlers class_id


let open_channel (t, channel_no) =
  Hashtbl.add t.channels channel_no
    { state = Ready;
      method_handlers = Hashtbl.create 0;
      content_handlers = Hashtbl.create 0;
    }

let flush t = Writer.flushed t.output

(** [writer] is channel 0 writer. It must be attached *)
let init ~port ~host =
  let addr = Tcp.to_host_and_port host port in
  Tcp.connect addr >>= fun (socket, input, output) ->
  Socket.setopt socket Socket.Opt.nodelay true;
  let t = { input; output; max_length = 256;
            channels = Hashtbl.create 0;
          }
  in
  open_channel (t, 0);
  Deferred.forever t read_frame;
  Writer.write output protocol_header;
  return t

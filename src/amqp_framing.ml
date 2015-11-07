(** Internal *)

module P = Printf
open Async.Std
open Core.Std
open Amqp_types
open Amqp_protocol
open Amqp_protocol.Spec
open Amqp_io

exception Unknown_frame_type of int
exception Connection_closed
exception Busy
exception Unhandled_method of message_id
exception Unhandled_header of class_id

type channel_no = int

type channel_state =
  | Ready
  | Waiting of class_id * Input.t * int * Bytes.t

type message =
  | Method of message_id * Input.t
  | Content of class_id * Input.t * string

type data = Input.t

type content_handler = data * string -> unit
type method_handler = data -> unit

type channel = { mutable state: channel_state;
                 method_handlers: (message_id, method_handler) Hashtbl.t;
                 content_handlers: (class_id, content_handler) Hashtbl.t;
                 writer: Bigstring.t Pipe.Writer.t;
               }

type t = { input: Reader.t; output: Writer.t;
           multiplex: Bigstring.t Pipe.Reader.t Pipe.Writer.t;
           channels: (channel_no, channel) Hashtbl.t;
           mutable max_length: int;
           id: string
         }

let protocol_header = "AMQP\x00\x00\x09\x01"
let read_method_frame = read (Short :: Short :: Nil)
let read_content_header = read (Short :: Short :: Longlong :: Nil)

(* Should register a monitor *)
let (>>) a b =
  a >>= function `Eof _ -> raise Connection_closed
               | `Ok -> b ()

let read_frame_header, write_frame_header =
  let open Amqp_protocol.Spec in
  let spec = Octet :: Short :: Long :: Nil in
  read spec, write spec


let size_of_writer writer =
  let sizer = Output.sizer () in
  writer sizer;
  Output.size sizer

let write_frame t channel_no tpe writer =
  let channel = Hashtbl.find_exn t.channels channel_no in
  let length = size_of_writer writer in
  let output = Output.create (1+2+4+length+1) in

  write_frame_header output tpe channel_no length
  |> writer
  |> fun w -> Output.octet w Amqp_constants.frame_end;
  Pipe.write_without_pushback channel.writer (Output.get output)

let write_method_id =
  let open Amqp_protocol.Spec in
  write (Short :: Short :: Nil)

let write_method (t, channel_no) (cid, mid) writer =
  log "Send method on channel: %d (%d, %d)" channel_no cid mid;
  let writer output =
    write_method_id output cid mid
    |> writer
  in
  write_frame t channel_no Amqp_constants.frame_method writer

let write_content_header =
  let open Amqp_protocol.Spec in
  write (Short :: Short :: Longlong :: Nil)

let write_content (t, channel_no) class_id writer data =
  log "Send content on channel: %d (%d)" channel_no class_id;
  let writer output =
    write_content_header output class_id 0 (String.length data)
    |> writer
  in
  write_frame t channel_no Amqp_constants.frame_header writer;
  let length = String.length data in
  (* Here comes the data *)
  let rec send offset =
    if offset < length then
      let size = min t.max_length (length - offset) in
      write_frame t channel_no Amqp_constants.frame_body
        (fun output -> Output.string output ~src_pos:offset ~len:size data; output);
      send (offset + t.max_length)
    else
      ()
  in
  send 0

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no size input =
  let channel = Hashtbl.find_exn t.channels channel_no in
  match channel.state, tpe with
  | Ready, n when n = Amqp_constants.frame_method ->
    (* Standard method message *)
    let message_id = read_method_frame (fun a b -> a, b) input in
    log "Received method on channel: %d (%d, %d)" channel_no (fst message_id) (snd message_id);
    begin
      match Hashtbl.find channel.method_handlers message_id with
      | Some handler -> handler input
      | None ->
        P.eprintf "No handler: (%d, %d)\n%!" (fst message_id) (snd message_id);
        raise (Unhandled_method message_id)
    end;
    Input.destroy input
  | Ready, n when n = Amqp_constants.frame_header ->
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in
    log "Received header on channel: %d (%d). Size: %d" channel_no class_id size;

    if size = 0 then begin
      log "Received body on channel: %d (%d)" channel_no class_id;
      match Hashtbl.find channel.content_handlers class_id with
        | Some handler ->
          handler (input, "")
        | None -> raise (Unhandled_header class_id)
      end
    else
      channel.state <- Waiting (class_id, input, 0, String.create size)
  | Waiting (class_id, content, offset, buffer), n when n = Amqp_constants.frame_body ->
    log "Received body data on channel: %d (%d) " channel_no class_id;
    Input.copy input ~dst_pos:offset ~len:size buffer;
    Input.destroy input;
    if (String.length buffer = offset + size) then begin
      channel.state <- Ready;
      log "Received body on channel: %d (%d)" channel_no class_id;
      match Hashtbl.find channel.content_handlers class_id with
      | Some handler -> handler (content, buffer);
        Input.destroy content;
      | None -> raise (Unhandled_header class_id)
    end
    else
      channel.state <- Waiting (class_id, content, offset + size, buffer)
  | _, n when n = Amqp_constants.frame_heartbeat ->
    write_frame t 0 Amqp_constants.frame_heartbeat (fun i -> i)
  | _, n -> raise (Unknown_frame_type n)

let read_frame t =
  let buf = Bigstring.create (1+2+4) in
  Reader.really_read_bigsubstring t.input (Bigsubstring.create buf) >> fun () ->
  let input = Input.init buf in
  let tpe, channel_no, length = read_frame_header (fun a b c -> a,b,c) input in
  Bigstring.unsafe_destroy buf;

  let buf = Bigstring.create (length+1) in
  Reader.really_read_bigsubstring t.input (Bigsubstring.create buf) >> fun () ->
  begin match Bigstring.get buf length |> Char.to_int with
    | n when n = Amqp_constants.frame_end ->
      let input = Input.init buf in
      decode_message t tpe channel_no length input
    | n -> failwith (Printf.sprintf "Unexpected frame end: %x" n)
  end;
  return t

let register_method_handler (t, channel_no) message_id handler =
  let c = Hashtbl.find_exn t.channels channel_no in
  match Hashtbl.add c.method_handlers ~key:message_id ~data:handler with
  | `Ok -> ()
  | `Duplicate -> raise Busy

let register_content_handler (t, channel_no) class_id handler =
  let c = Hashtbl.find_exn t.channels channel_no in
  match Hashtbl.add c.content_handlers ~key:class_id ~data:handler with
  | `Ok -> ()
  | `Duplicate -> raise Busy

let deregister_method_handler (t, channel_no) message_id =
  let c = Hashtbl.find_exn t.channels channel_no in
  if not (Hashtbl.mem c.method_handlers message_id) then raise Busy;
  Hashtbl.remove c.method_handlers message_id

let deregister_content_handler (t, channel_no) class_id =
  let c = Hashtbl.find_exn t.channels channel_no in
  if not (Hashtbl.mem c.content_handlers class_id) then raise Busy;
  Hashtbl.remove c.content_handlers class_id


let open_channel t channel_no =
  let reader, writer = Pipe.create () in
  Hashtbl.add t.channels ~key:channel_no
    ~data:{ state = Ready;
            method_handlers = Hashtbl.create ~growth_allowed:true ~hashable:Hashtbl.Hashable.poly ();
            content_handlers = Hashtbl.create ~growth_allowed:true ~hashable:Hashtbl.Hashable.poly ();
            writer;
          }
  |> ignore;
  Pipe.write t.multiplex reader

let close_channel t channel_no =
  let channel = Hashtbl.find_exn t.channels channel_no in
  Pipe.close channel.writer;
  Hashtbl.remove t.channels channel_no

let flush t = Writer.flushed t.output

let rec start_writer output channels =
  Pipe.read channels >>= function
  | `Ok data ->
    Async_unix.Writer.write_bigstring output data;
    (* TODO: Flush and destroy *)
    start_writer output channels
  | `Eof -> return ()

let id {id; _} = id

(** [writer] is channel 0 writer. It must be attached *)
let init ~id ~port host =
  let id = Printf.sprintf "%s.%s.%s.%s" id (Unix.gethostname ()) (Unix.getpid () |> Core.Std.Pid.to_string) (Sys.executable_name |> Filename.basename) in
  let addr = Tcp.to_host_and_port host port in
  Tcp.connect addr >>= fun (socket, input, output) ->
  Socket.setopt socket Socket.Opt.nodelay true;
  let reader, writer = Pipe.create () in
  don't_wait_for (start_writer output (Pipe.interleave_pipe reader));
  let t =
    { input; output;
      max_length = 256; (* TODO: Use actual framing size *)
      channels = Hashtbl.create ~growth_allowed:true ~hashable:Hashtbl.Hashable.poly ();
      multiplex = writer;
      id;
    }
  in
  Writer.write output protocol_header;
  Deferred.forever t read_frame;
  open_channel t 0 >>= fun () ->
  return t

let set_max_length t max_length =
  t.max_length <- max_length;

(** Internal *)

open Async.Std
open Core.Std
open Amqp_protocol
open Amqp_protocol.Spec
open Amqp_io

type channel_no = int

exception Unknown_frame_type of int
exception Connection_closed
exception Busy
exception No_handler_found
exception Illegal_channel of channel_no

type channel_state =
  | Ready
  | Waiting of Amqp_types.class_id * Input.t * int * Bytes.t

type message =
  | Method of Amqp_types.message_id * Input.t
  | Content of Amqp_types.class_id * Input.t * string

type data = Input.t

type content_handler = data * string -> unit
type method_handler = data -> unit

type channel = { mutable state: channel_state;
                 method_handlers: (Amqp_types.message_id * method_handler) Doubly_linked.t;
                 content_handlers: (Amqp_types.class_id * content_handler) Doubly_linked.t;
                 writer: Bigstring.t Pipe.Writer.t;
                 mutable ready: unit Ivar.t;
               }

type t = { input: Reader.t; output: Writer.t;
           multiplex: Bigstring.t Pipe.Reader.t Pipe.Writer.t;
           mutable channels: channel option Array.t;
           mutable max_length: int;
           id: string;
           mutable flow: bool;
         }

let protocol_header = "AMQP\x00\x00\x09\x01"
let read_method_frame = read (Short :: Short :: Nil)
let read_content_header = read (Short :: Short :: Longlong :: Nil)


let read_frame_header, write_frame_header =
  let open Amqp_protocol.Spec in
  let spec = Octet :: Short :: Long :: Nil in
  read spec, write spec


let channel t channel_no =
  match t.channels.(channel_no) with
  | None -> raise (Illegal_channel channel_no)
  | Some ch -> ch

let size_of_writer writer =
  let sizer = Output.sizer () in
  writer sizer;
  Output.size sizer

let write_frame t channel_no tpe writer =
  let channel = channel t channel_no in
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

let write_message (t, channel_no) (message_id, writer) content =
  let channel = channel t channel_no in
  match content with
  | Some (class_id, c_writer, data) ->
    Ivar.read channel.ready >>= fun () ->
    write_method (t, channel_no) message_id writer;
    write_content (t, channel_no) class_id c_writer data;
    return ()
  | None ->
    write_method (t, channel_no) message_id writer;
    return ()

let get_handler lst id =
  match Doubly_linked.find_elt ~f:(fun (id', _) -> id = id') lst with
  | Some elt -> Doubly_linked.move_to_front lst elt;
    snd (Doubly_linked.Elt.value elt)
  | None -> raise No_handler_found

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no size input =
  let channel = channel t channel_no in
  match channel.state, tpe with
  | Ready, n when n = Amqp_constants.frame_method ->
    (* Standard method message *)
    let message_id = read_method_frame (fun a b -> a, b) input in
    log "Received method on channel: %d (%d, %d)" channel_no (fst message_id) (snd message_id);
    let handler = get_handler channel.method_handlers message_id in
    handler input;
    Input.destroy input
  | Ready, n when n = Amqp_constants.frame_header ->
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in
    log "Received header on channel: %d (%d). Size: %d" channel_no class_id size;

    if size = 0 then begin
      log "Received body on channel: %d (%d)" channel_no class_id;
      let handler = get_handler channel.content_handlers class_id in
      handler (input, "")
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
      let handler = get_handler channel.content_handlers class_id in
      handler (content, buffer);
      Input.destroy content
    end
    else
      channel.state <- Waiting (class_id, content, offset + size, buffer)
  | _, n when n = Amqp_constants.frame_heartbeat ->
    write_frame t 0 Amqp_constants.frame_heartbeat (fun i -> i)
  | _, n -> raise (Unknown_frame_type n)

(** Cannot just keep running. It should terminate on close... However unexpected close should
    raise an error. What about a listener, that can be disabled? *)
let rec read_frame t =
  let buf = Bigstring.create (1+2+4) in
  Reader.really_read_bigsubstring t.input (Bigsubstring.create buf) >>= function
  | `Eof _ -> return ()
  | `Ok ->
    let input = Input.init buf in
    let tpe, channel_no, length = read_frame_header (fun a b c -> a,b,c) input in
    Bigstring.unsafe_destroy buf;

    let buf = Bigstring.create (length+1) in
    Reader.really_read_bigsubstring t.input (Bigsubstring.create buf) >>= function
    | `Eof _ -> return ()
    | `Ok -> match Bigstring.get buf length |> Char.to_int with
      | n when n = Amqp_constants.frame_end ->
        let input = Input.init buf in
        decode_message t tpe channel_no length input;
        read_frame t
      | n -> failwith (Printf.sprintf "Unexpected frame end: %x" n)

let register_method_handler (t, channel_no) message_id handler =
  let c = channel t channel_no in
  let (_: 'a Doubly_linked.Elt.t) = Doubly_linked.insert_first c.method_handlers (message_id, handler) in
  ()

let register_content_handler (t, channel_no) class_id handler =
  let c = channel t channel_no in
  let (_: 'a Doubly_linked.Elt.t) = Doubly_linked.insert_first c.content_handlers (class_id, handler) in
  ()

let deregister_method_handler (t, channel_no) message_id =
  let c = channel t channel_no in
  match Doubly_linked.find_elt c.method_handlers ~f:(fun (id, _ ) -> id = message_id) with
  | None -> raise Busy
  | Some elt -> Doubly_linked.remove c.method_handlers elt

let deregister_content_handler (t, channel_no) class_id =
  let c = channel t channel_no in
  match Doubly_linked.find_elt c.content_handlers ~f:(fun (id, _ ) -> id = class_id) with
  | None -> raise Busy
  | Some elt -> Doubly_linked.remove c.content_handlers elt

let set_flow t channel_no active =
  let c = channel t channel_no in
  match active with
  | true ->
    if Ivar.is_full c.ready then
      c.ready <- Ivar.create ()
  | false ->
    Ivar.fill_if_empty c.ready ()

let set_flow_all t active =
  t.flow <- active;
  Array.iteri t.channels
    ~f:(fun i -> function None -> ()
                        | Some _ -> set_flow t i active)

let open_channel t channel_no =
  (* Grow the array if needed *)
  let len = Array.length t.channels in
  if (len <= channel_no) then
    t.channels <- Array.append t.channels (Array.create ~len None);

  let reader, writer = Pipe.create () in
  let ready = match t.flow with
    | true -> Ivar.create ()
    | false -> Ivar.create_full ()
  in
  t.channels.(channel_no) <-
    Some { state = Ready;
           method_handlers = Doubly_linked.create ();
           content_handlers = Doubly_linked.create ();
           writer;
           ready;
         };

  Pipe.write t.multiplex reader

let flush t =
  Array.to_list t.channels
  |> List.filter_opt
  |> List.map ~f:(fun channel -> Pipe.downstream_flushed channel.writer >>= fun _ -> return ())
  |> Deferred.all_unit >>= fun () ->
  Writer.flushed t.output

let flush_channel t channel_no =
  let channel = channel t channel_no in
  Pipe.downstream_flushed channel.writer >>= fun _ ->
  Writer.flushed t.output

let close t =
  Array.to_list t.channels
  |> List.filter_opt
  |> List.iter ~f:(fun ch -> Pipe.close ch.writer);
  flush t >>= fun () ->
  Reader.close t.input >>= fun () ->
  Writer.close t.output >>= fun () ->
  return ()

let close_channel t channel_no =
  let channel = channel t channel_no in
  t.channels.(channel_no) <- None;
  Pipe.close channel.writer;
  Pipe.downstream_flushed channel.writer >>= fun _ ->
  flush t

let rec start_writer output channels =
  Pipe.read channels >>= function
  | `Ok data ->
    Async_unix.Writer.write_bigstring output data;
    (* TODO: Flush and destroy *)
    start_writer output channels
  | `Eof -> return ()

let id {id; _} = id

(** [writer] is channel 0 writer. It must be attached *)
let init ~id input output  =
  let id = Printf.sprintf "%s.%s.%s.%s" id (Unix.gethostname ()) (Unix.getpid () |> Pid.to_string) (Sys.executable_name |> Filename.basename) in
  let reader, writer = Pipe.create () in
  don't_wait_for (start_writer output (Pipe.interleave_pipe reader));
  let t =
    { input; output;
      max_length = 1024;
      channels = Array.create ~len:256 None;
      multiplex = writer;
      id;
      flow = false;
    }
  in
  Writer.write output protocol_header;
  Deferred.don't_wait_for (read_frame t);
  open_channel t 0 >>= fun () ->
  return t

let set_max_length t max_length =
  t.max_length <- max_length;

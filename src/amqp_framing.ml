(** Internal *)
open Amqp_thread
open Amqp_protocol
open Amqp_io
module S = Amqp_protocol.Spec

type channel_no = int

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
                 mutable method_handlers: (Amqp_types.message_id * method_handler) list;
                 mutable content_handlers: (Amqp_types.class_id * content_handler) list;
                 writer: String.t Pipe.Writer.t;
                 mutable ready: unit Ivar.t;
               }

type t = { input: Reader.t; output: Writer.t;
           multiplex: String.t Pipe.Reader.t Pipe.Writer.t;
           mutable channels: channel option array;
           mutable max_length: int;
           id: string;
           mutable flow: bool;
         }

let protocol_header = "AMQP\x00\x00\x09\x01"
let read_method_frame = S.read S.(Short :: Short :: Nil)
let read_content_header = S.read S.(Short :: Short :: Longlong :: Nil)


let read_frame_header, write_frame_header =
  let open Amqp_protocol.Spec in
  let spec = Octet :: Short :: Long :: Nil in
  read spec, write spec


let channel t channel_no =
  match t.channels.(channel_no) with
  | None -> raise (Amqp_types.Channel_not_found channel_no)
  | Some ch -> ch

let size_of_writer writer =
  let sizer = Output.sizer () in
  writer sizer;
  Output.size sizer

let create_frame channel_no tpe writer =
  let length = size_of_writer writer in
  let output = Output.create (1+2+4+length+1) in

  write_frame_header output tpe channel_no length
  |> writer
  |> fun w -> Output.octet w Amqp_constants.frame_end;
  Output.get output

let write_method_id =
  let open Amqp_protocol.Spec in
  write (Short :: Short :: Nil)

let create_method_frame channel_no (cid, mid) writer =
  log "Send method on channel: %d (%d, %d)" channel_no cid mid;
  let writer output =
    write_method_id output cid mid
    |> writer
  in
  create_frame channel_no Amqp_constants.frame_method writer

let create_content_header =
  let open Amqp_protocol.Spec in
  write (Short :: Short :: Longlong :: Nil)

let add_content_frames queue max_length channel_no class_id writer data =
  log "Send content on channel: %d (%d)" channel_no class_id;

  let writer output =
    create_content_header output class_id 0 (String.length data)
    |> writer
  in
  let msg = create_frame channel_no Amqp_constants.frame_header writer in
  Queue.add msg queue;

  let length = String.length data in

  (* Here comes the data *)
  let rec send offset =
    if offset < length then
      let size = min max_length (length - offset) in
      let msg =
        create_frame channel_no Amqp_constants.frame_body
          (fun output -> Output.string output ~src_pos:offset ~len:size data; output)
      in
      Queue.add msg queue;
      send (offset + max_length)
    else
      ()
  in
  send 0

(* This should write to the pipe using write', and return a deferred (which is does already, just better *)
let write_message (t, channel_no) (message_id, writer) content =
  let channel = channel t channel_no in
  match content with
  | Some (class_id, c_writer, data) ->
    Ivar.read channel.ready >>= fun () ->
    let frames = Queue.create () in
    let msg = create_method_frame channel_no message_id writer in
    Queue.add msg frames;
    add_content_frames frames t.max_length channel_no class_id c_writer data;
    Pipe.transfer_in channel.writer ~from:frames
  | None ->
    create_method_frame channel_no message_id writer
    |> Pipe.write channel.writer

let send_heartbeat t =
  let channel = channel t 0 in
  create_frame 0 Amqp_constants.frame_heartbeat (fun i -> i)
  |> Pipe.write channel.writer

let get_handler id = function
  | ((id', h) :: _) as lst when id = id' -> (h, lst)
  | lst ->
      begin
        let elt = ref None in
        let rec inner = function
          | [] -> []
          | ((id', _) as e) :: xs when id = id' ->
              elt := Some e;
              xs
          | x :: xs -> x :: inner xs
        in
        let tail = inner lst in
        match !elt with
        | None -> raise Amqp_types.No_handler_found
        | Some e -> snd e, e :: tail
      end

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no size input =
  let channel = channel t channel_no in
  match channel.state, tpe with
  | Ready, n when n = Amqp_constants.frame_method ->
    (* Standard method message *)
    let message_id = read_method_frame (fun a b -> a, b) input in
    log "Received method on channel: %d (%d, %d)" channel_no (fst message_id) (snd message_id);
    let (handler, handlers) = get_handler message_id channel.method_handlers in
    channel.method_handlers <- handlers; (* Move front *)
    handler input;
  | Ready, n when n = Amqp_constants.frame_header ->
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in
    log "Received header on channel: %d (%d). Size: %d" channel_no class_id size;

    if size = 0 then begin
      log "Received body on channel: %d (%d)" channel_no class_id;
      let handler, handlers = get_handler class_id channel.content_handlers in
      channel.content_handlers <- handlers; (* Move front *)
      handler (input, "")
    end
    else
      channel.state <- Waiting (class_id, input, 0, Bytes.create size)
  | Waiting (class_id, content, offset, buffer), n when n = Amqp_constants.frame_body ->
    log "Received body data on channel: %d (%d) " channel_no class_id;
    Input.copy input ~dst_pos:offset ~len:size buffer;
    if (String.length buffer = offset + size) then begin
      channel.state <- Ready;
      log "Received body on channel: %d (%d)" channel_no class_id;
      let handler, handlers = get_handler class_id channel.content_handlers in
      channel.content_handlers <- handlers; (* Move front *)
      handler (content, buffer);
    end
    else
      channel.state <- Waiting (class_id, content, offset + size, buffer)
  | _, n when n = Amqp_constants.frame_heartbeat -> ()
  | _, n -> raise (Amqp_types.Unknown_frame_type n)

(** Cannot just keep running. It should terminate on close... However unexpected close should
    raise an error. What about a listener, that can be disabled? *)
let rec read_frame t =
  let buf = Bytes.create (1+2+4) in
  Reader.really_read t.input buf >>= function
  | `Eof _ -> return ()
  | `Ok ->
    let input = Input.init buf in
    let tpe, channel_no, length = read_frame_header (fun a b c -> a, b, c) input in
    let buf = Bytes.create (length+1) in
    Reader.really_read t.input buf >>= function
    | `Eof _ -> return ()
    | `Ok -> match buf.[length] |> Char.code with
      | n when n = Amqp_constants.frame_end ->
        let input = Input.init buf in
        decode_message t tpe channel_no length input;
        read_frame t
      | n -> failwith (Printf.sprintf "Unexpected frame end: %x" n)

let register_method_handler (t, channel_no) message_id handler =
  let c = channel t channel_no in
  if List.exists (fun x -> fst x = message_id) c.method_handlers then
    raise Amqp_types.Busy;
  c.method_handlers <- (message_id, handler) :: c.method_handlers

let register_content_handler (t, channel_no) class_id handler =
  let c = channel t channel_no in
  if List.exists (fun x -> fst x = class_id) c.content_handlers then
    raise Amqp_types.Busy;
  c.content_handlers <- (class_id, handler) :: c.content_handlers

let rec remove_handler id = function
  | (id', _) :: xs when id = id' -> xs
  | x :: xs -> x :: remove_handler id xs
  | [] -> (* No handler removed *)
      raise Amqp_types.Busy

let deregister_method_handler (t, channel_no) message_id =
  let c = channel t channel_no in
  let handlers = remove_handler message_id c.method_handlers in
  c.method_handlers <- handlers

let deregister_content_handler (t, channel_no) class_id =
  let c = channel t channel_no in
  let handlers = remove_handler class_id c.content_handlers in
  c.content_handlers <- handlers

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
  Array.iteri (fun i _ -> set_flow t i active) t.channels

let open_channel t channel_no =
  (* Grow the array if needed *)
  let len = Array.length t.channels in
  if (len <= channel_no) then
    t.channels <- Array.append t.channels (Array.make len None);

  let reader, writer = Pipe.create () in
  Pipe.set_size_budget writer 4;
  let ready = match t.flow with
    | true -> Ivar.create ()
    | false -> Ivar.create_full ()
  in
  t.channels.(channel_no) <-
    Some { state = Ready;
           method_handlers = [];
           content_handlers = [];
           writer;
           ready;
         };

  Pipe.write t.multiplex reader

let flush t =
  Array.to_list t.channels
  |> List.map (function None -> return () | Some channel -> Pipe.flush channel.writer >>= fun _ -> return ())
  |> Deferred.all_unit >>= fun () ->
  Writer.flush t.output

let flush_channel t channel_no =
  let channel = channel t channel_no in
  Pipe.flush channel.writer >>= fun _ ->
  Writer.flush t.output

let close t =
  let l = Array.to_list t.channels in
  Deferred.List.iter ~f:(function None -> return () | Some ch -> Pipe.close ch.writer) l >>= fun () ->
  Reader.close t.input >>= fun () ->
  Writer.close t.output >>= fun () ->
  return ()

let close_channel t channel_no =
  let channel = channel t channel_no in
  t.channels.(channel_no) <- None;
  Pipe.close channel.writer >>= fun _ ->
  flush t

let rec start_writer output channels =
  Pipe.read channels >>= function
  | `Ok data ->
    Writer.write output data;
    start_writer output channels
  | `Eof -> return ()

let id {id; _} = id

(** [writer] is channel 0 writer. It must be attached *)
let init ~id input output  =
  let id = Printf.sprintf "%s.%s.%s.%s" id (Unix.gethostname ()) (Unix.getpid () |> string_of_int) (Sys.executable_name |> Filename.basename) in
  let reader, writer = Pipe.create () in
  spawn (start_writer output (Pipe.interleave_pipe reader));
  let t =
    { input; output;
      max_length = 1024;
      channels = Array.make 256 None;
      multiplex = writer;
      id;
      flow = false;
    }
  in
  Writer.write output protocol_header;
  spawn (read_frame t);
  open_channel t 0 >>= fun () ->
  return t

let set_max_length t max_length =
  t.max_length <- max_length;

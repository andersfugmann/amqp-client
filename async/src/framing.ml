(** Internal *)
open Thread
open Amqp_client_lib
module S = Protocol.Spec

type channel_no = int

type channel_state =
  | Ready
  | Waiting of Types.class_id * Io.Input.t * int * Bytes.t

type message =
  | Method of Types.message_id * Io.Input.t
  | Content of Types.class_id * Io.Input.t * string

type data = Io.Input.t

type content_handler = data * string -> unit
type method_handler = data -> unit

type channel = { mutable state: channel_state;
                 method_handlers: (Types.message_id * method_handler) Mlist.t;
                 content_handlers: (Types.class_id * content_handler) Mlist.t;
                 writer: String.t Pipe.Writer.t;
                 mutable ready: unit Ivar.t;
               }

type close_handler = string -> unit Deferred.t
type t = { input: Reader.t; output: Writer.t;
           multiplex: String.t Pipe.Reader.t Pipe.Writer.t;
           multiplex_reader: String.t Pipe.Reader.t Pipe.Reader.t;
           mutable channels: channel option array;
           mutable max_length: int;
           id: string;
           mutable flow: bool;
         }

let protocol_header = "AMQP\x00\x00\x09\x01"
let read_method_frame = S.read S.(Short :: Short :: [])
let read_content_header = S.read S.(Short :: Short :: Longlong :: [])


let read_frame_header, write_frame_header =
  let open Protocol.Spec in
  let spec = Octet :: Short :: Long :: [] in
  read spec, write spec

let channel t channel_no =
  match t.channels.(channel_no) with
  | None -> raise (Types.Channel_not_found channel_no)
  | Some ch -> ch

let size_of_writer writer =
  Io.Output.sizer ()
  |> writer
  |> Io.Output.size

let create_frame channel_no tpe writer =
  let length = size_of_writer writer in
  let output = Io.Output.create (1+2+4+length+1) in

  write_frame_header output tpe channel_no length
  |> writer
  |> fun w -> Io.Output.octet w Constants.frame_end;
  Io.Output.get output
  |> Bytes.unsafe_to_string

let write_method_id =
  let open Protocol.Spec in
  write (Short :: Short :: [])

let create_method_frame channel_no (cid, mid) writer =
  let writer output =
    write_method_id output cid mid
    |> writer
  in
  create_frame channel_no Constants.frame_method writer

let create_content_header =
  let open Protocol.Spec in
  write (Short :: Short :: Longlong :: [])

let add_content_frames queue max_length channel_no class_id writer data =
  let length = String.length data in
  let writer output =
    create_content_header output class_id 0 length
    |> writer
  in
  let msg = create_frame channel_no Constants.frame_header writer in
  Ocaml_lib.Queue.add msg queue;

  (* Send the data *)
  let rec send offset =
    if offset < length then
      let size = min max_length (length - offset) in
      let msg =
        create_frame channel_no Constants.frame_body
          (fun output -> Io.Output.string output ~src_pos:offset ~len:size data; output)
      in
      Ocaml_lib.Queue.add msg queue;
      send (offset + max_length)
    else
      ()
  in
  send 0

let write_message (t, channel_no) (message_id, writer) content =
  let channel = channel t channel_no in
  match content with
  | Some (class_id, c_writer, data) ->
    Ivar.read channel.ready >>= fun () ->
    let frames = Ocaml_lib.Queue.create () in
    let msg = create_method_frame channel_no message_id writer in
    Ocaml_lib.Queue.add msg frames;
    add_content_frames frames t.max_length channel_no class_id c_writer data;
    Pipe.transfer_in channel.writer ~from:frames
  | None ->
    create_method_frame channel_no message_id writer
    |> Pipe.write channel.writer

let send_heartbeat t =
  let channel = channel t 0 in
  create_frame 0 Constants.frame_heartbeat (fun i -> i)
  |> Pipe.write channel.writer

(** read_frame reads a frame from the input, and sends the data to
    the channel writer *)
let decode_message t tpe channel_no size input =
  let channel = channel t channel_no in
  match channel.state, tpe with
  | Ready, n when n = Constants.frame_method ->
    (* Standard method message *)
    let message_id = read_method_frame (fun a b -> a, b) input in
    let handler =
      Mlist.take ~pred:(fun elt -> fst elt = message_id) channel.method_handlers
      |> Option.get_exn ~exn:Types.No_handler_found
      |> snd
    in
    Mlist.prepend channel.method_handlers (message_id, handler);
    handler input;
  | Ready, n when n = Constants.frame_header ->
    let class_id, _weight, size =
      read_content_header (fun a b c -> a, b, c) input
    in

    if size = 0 then begin
      let handler =
        Mlist.take ~pred:(fun elt -> fst elt = class_id) channel.content_handlers
        |> Option.get_exn ~exn:Types.No_handler_found
        |> snd
      in
      Mlist.prepend channel.content_handlers (class_id, handler);
      handler (input, "")
    end
    else
      channel.state <- Waiting (class_id, input, 0, Bytes.create size)
  | Waiting (class_id, content, offset, buffer), n when n = Constants.frame_body ->
    Io.Input.copy input ~dst_pos:offset ~len:size buffer;
    if (Bytes.length buffer = offset + size) then begin
      channel.state <- Ready;
      let handler =
        Mlist.take ~pred:(fun elt -> fst elt = class_id) channel.content_handlers
        |> Option.get_exn ~exn:Types.No_handler_found
        |> snd
      in
      Mlist.prepend channel.content_handlers (class_id, handler);
      handler (content, Bytes.unsafe_to_string buffer);
    end
    else
      channel.state <- Waiting (class_id, content, offset + size, buffer)
  | _, n when n = Constants.frame_heartbeat -> ()
  | _, n -> raise (Types.Unknown_frame_type n)

let rec read_frame t close_handler =
  let header = Bytes.create (1+2+4) in
  Reader.read t.input header >>= function
  | `Eof n ->
      close_handler (Bytes.sub_string header 0 n)
  | `Ok ->
    let input = Io.Input.init (Bytes.unsafe_to_string header) in
    let tpe, channel_no, length = read_frame_header (fun a b c -> a, b, c) input in
    let buf = Bytes.create (length+1) in
    Reader.read t.input buf >>= function
    | `Eof n ->
        let s = Bytes.extend header 0 n in
        Bytes.blit buf 0 s (1+2+4) n;
        close_handler (Bytes.to_string s)
    | `Ok -> match Bytes.get buf length |> Char.code with
      | n when n = Constants.frame_end ->
        let input = Io.Input.init (Bytes.unsafe_to_string buf) in
        decode_message t tpe channel_no length input;
        read_frame t close_handler
      | n -> failwith (Printf.sprintf "Unexpected frame end: %x" n)

let register_method_handler (t, channel_no) message_id handler =
  let c = channel t channel_no in
  Mlist.append c.method_handlers (message_id, handler)

let register_content_handler (t, channel_no) class_id handler =
  let c = channel t channel_no in
  Mlist.prepend c.content_handlers (class_id, handler)

let deregister_method_handler (t, channel_no) message_id =
  let c = channel t channel_no in
  let (_ : 'a option) = Mlist.take ~pred:(fun (id, _) -> id = message_id) c.method_handlers in
  ()

let deregister_content_handler (t, channel_no) class_id =
  let c = channel t channel_no in
  let (_ : 'a option) = Mlist.take ~pred:(fun (id, _) -> id = class_id) c.content_handlers in
  ()

let set_flow_on_channel c = function
  | true ->
      if Ivar.is_full c.ready then
        c.ready <- Ivar.create ()
  | false ->
      Ivar.fill_if_empty c.ready ()


let set_flow t channel_no active =
  let c = channel t channel_no in
  set_flow_on_channel c active

let set_flow_all t active =
  t.flow <- active;
  Array.iter (function Some c -> set_flow_on_channel c active | None -> ()) t.channels

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
           method_handlers = Mlist.create ();
           content_handlers = Mlist.create ();
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
let init ~id input output =
  let id = Printf.sprintf "%s.%s.%s.%s" id (Unix.gethostname ()) (Unix.getpid () |> string_of_int) (Sys.executable_name |> Filename.basename) in
  let reader, writer = Pipe.create () in
  { input;
    output;
    max_length = 1024;
    channels = Array.make 256 None;
    multiplex = writer;
    multiplex_reader = reader;
    id;
    flow = false;
  }

let start t close_handler =
  let exn_handler exn = close_handler (Printexc.to_string exn) in
  spawn ~exn_handler (start_writer t.output (Pipe.interleave_pipe t.multiplex_reader));
  Writer.write t.output protocol_header;
  spawn ~exn_handler (read_frame t close_handler);
  open_channel t 0

let set_max_length t max_length =
  t.max_length <- max_length;

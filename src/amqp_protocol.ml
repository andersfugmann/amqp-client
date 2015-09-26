open Batteries
open Amqp_types

exception Unknown_frame of int
exception Decode_error


let protocol_header = "AMQP\x00\x00\x09\x01"

let peer_properties = Table
type peer_properties = table

module Transport = struct
  type t = { i: IO.input; o: unit IO.output; written: (unit -> int)}

  let connect () =
    let i, o = Unix.(open_connection ~autoclose:false (ADDR_INET (inet_addr_loopback, 5672))) in
    IO.write_string o protocol_header;
    IO.flush o;
    let o, written = IO.pos_out o in
    { i; o; written }

end

let print_bytes bytes =
  String.iter (fun c -> Printf.printf "%02x " (Char.code c)) bytes;
  Printf.printf "\n%!";

module Framing = struct

  (* Imperative. We will change that later *)
  type hdr = (int * int)
  type channel_id = int

  type channel_state =
    | Ready
    | Waiting of hdr * int * Buffer.t

  type message =
    | Method of string
    | Body of string

  type callback = hdr -> message -> unit

  type channel = {  callback: callback;
                    mutable state: channel_state;
                 }

  let channels : (int, channel) Hashtbl.t = Hashtbl.create 0

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

  let write output channel (hdr, message) =
    let (data, tpe) = match message with
      | Method data -> data, Amqp_spec.frame_method
      | Body data -> data, Amqp_spec.frame_header (* TODO *)
    in
    let hdr_data =
      let o = Tuple2.uncurry (write method_frame (IO.output_string ())) hdr in
      (IO.close_out o)
    in
    let os = IO.output_string () in
    write_frame os tpe channel (hdr_data ^ data) frame_end |> ignore;
    let bytes = IO.close_out os in
    print_bytes bytes;
    IO.nwrite output bytes;
    IO.flush output

  let read input =
    let (tpe, channel_id, data, magic) = read_frame (Tuple4.curry identity) input in
    assert (magic = frame_end);
    Printf.printf "Channel: %d\n%!" channel_id;
    let channel = Hashtbl.find channels channel_id in
    match tpe with
    | n when n = Amqp_spec.frame_method ->
      (* Standard method message *)
      assert (channel.state = Ready);
      let hdr = read_method_frame (Tuple2.curry identity) (IO.input_string data) in
      let data = String.slice ~first:4 data in
      channel.callback hdr (Method data)
    | n when n = Amqp_spec.frame_header ->
      assert (channel.state = Ready);
      (* Decode the header frame *)
      let cls_id, mth_id, size, _magic =
        read_content_header
          (Tuple4.curry identity)
          (IO.input_string data)
      in
      channel.state <- Waiting ((cls_id, mth_id), size, Buffer.create size)
    | n when n = Amqp_spec.frame_body ->
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
    | n when n = Amqp_spec.frame_heartbeat ->
      ()
    | n -> raise (Unknown_frame n)

  let register_callback channel callback =
    Hashtbl.add channels channel { callback; state = Ready }

end


(* Uh. This is a cool method *)
let reply (_class, _method, spec, make, _apply) (r_class, r_method, r_spec, _r_make, r_apply) =
  let read = read spec in
  let write = write r_spec in
  fun callback buf ->
    let req = read make (IO.input_string buf) in
    let rep = callback req in
    let out = r_apply (write (IO.output_string ())) rep in
    ( (r_class, r_method), Framing.Method (IO.close_out out) )

module Start = struct
  let handle = reply Amqp_spec.Connection.Start.def Amqp_spec.Connection.Start_ok.def
end

let handle_start {Amqp_spec.Connection.Start.version_major;
                  version_minor;
                  server_properties;
                  mechanisms;
                  locales } =
  Printf.printf "Connection start\n";
  Printf.printf "Id: %d %d\n" version_major version_minor;
  Printf.printf "Properties: %s(%d)\n" (dump server_properties) (List.length server_properties);
  Printf.printf "Mechanisms: %s\n" mechanisms;
  Printf.printf "Locales: %s\n" locales;
  {
    Amqp_spec.Connection.Start_ok.client_properties = server_properties;
    mechanism = "PLAIN";(*String.nsplit ~by:" " mechanisms |> List.hd; *)
    response = "\x00guest\x00guest";
    locale = String.nsplit ~by:";" locales |> List.hd
  }

(* How do we send a frame? *)
let dispatch o channel hdr message =
  let data = match message with
    | Framing.Method data -> data
    | Framing.Body _ -> failwith "Unsupported"
  in
  let reply =
    match hdr with
    | (10, 10)  ->
      Some (Start.handle handle_start data)
    | _ -> failwith "Not suported"
  in
  (* Ahhh. This is where we handle it all. *)
  match reply with
  | Some message -> Framing.write o channel message
  | None -> ()


let init () =
  let t = Transport.connect () in
  print_endline "Connected";
  Framing.register_callback 0 (dispatch t.Transport.o 0);
  Framing.read t.Transport.i;
  Printf.printf "Written: %d\n" (t.Transport.written ());
  Framing.read t.Transport.i;
  ()

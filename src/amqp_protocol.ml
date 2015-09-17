open Batteries
open Amqp_types

exception Unknown_frame of int
exception Decode_error


let protocol_header = "AMQP\x00\x00\x09\x01"

module Transport = struct
  type t = { i: IO.input; o: unit IO.output; }

  let connect () =
    let i, o = Unix.(open_connection ~autoclose:false (ADDR_INET (inet_addr_loopback, 5672))) in
    IO.write_string o protocol_header;
    IO.flush o;
    { i; o }

end

module Framing = struct

  (* Imperative. We will change that later *)
  type hdr = { class_id: int; method_id: int }
  let hdr class_id method_id = { class_id; method_id }

  type channel_state =
    | Ready
    | Waiting of hdr * int * Buffer.t

  type message =
    | Method of string
    | Body of string

  type callback = hdr * message -> unit

  type channel = {  callback: callback;
                    mutable state: channel_state;
                 }

  let channels : (int, channel) Hashtbl.t = Hashtbl.create 0

  let frame          = Octet @ Short @ Longstr @ Octet @ eol
  let scan_frame     = scan frame
  let print_frame : (string IO.output -> 'a) = print frame

  let method_frame   = Short @ Short @ eol
  let content_header = Short @ Short @ Longlong @ Short @ eol

  let scan_method_frame = scan method_frame
  let print_method_frame : (string IO.output -> 'a) = print method_frame

  let scan_content_header = scan content_header
  let print_content_header : (string IO.output -> 'a) = print content_header

  let read_frame input =
    let (tpe, channel, data, _magic) = scan_frame (Tuple4.curry identity) input in
    Printf.printf "Channel: %d\n%!" channel;
    let channel = Hashtbl.find channels channel in
    match tpe with
    | n when n = Amqp_spec.frame_method ->
      (* Standard method message *)
      assert (channel.state = Ready);
      let hdr = scan_method_frame hdr (IO.input_string data) in
      let data = String.slice ~first:4 data in
      channel.callback (hdr, Method data)
    | n when n = Amqp_spec.frame_header ->
      assert (channel.state = Ready);
      (* Decode the header frame *)
      let hdr, size =
        scan_content_header
          (fun a b c _ -> hdr a b, c)
          (IO.input_string data)
      in
      channel.state <- Waiting (hdr, size, Buffer.create size);
    | n when n = Amqp_spec.frame_body ->
      begin
      match channel.state with
        | Ready -> failwith "Channel not expecting data frames"
        | Waiting (hdr, size, buffer) ->
          Buffer.add_string buffer data;
          if (size <= Buffer.length buffer) then begin
            channel.callback (hdr, Body (Buffer.contents buffer));
            channel.state <- Ready
          end
      end
    | n when n = Amqp_spec.frame_heartbeat ->
      ()
    | n -> raise (Unknown_frame n)

  let register_callback channel callback =
    Hashtbl.add channels channel { callback; state = Ready }


end

module Test = struct
  let method_frame = Short @ Short @ eol


  let test_for_identity () =
    let (i, o) = IO.pipe () in
    let () = print method_frame o 5 6 in
    let (a, b) = scan method_frame (fun a b -> a,b) i in

    assert ((a, b) = (5,6))
end






let print_data (hdr, data) =
  Printf.printf "Data: (%d, %d) Len: %d\n%!"
    hdr.Framing.class_id
    hdr.Framing.method_id
    (String.length (match data with Framing.Method d | Framing.Body d -> d))

let init () =
  let t = Transport.connect () in
  print_endline "Connected";
  Framing.register_callback 0 print_data;
  Framing.read_frame t.Transport.i

open Batteries
open Amqp_types

exception Unknown_frame of int
exception Decode_error


let protocol_header = "AMQP\x00\x00\x09\x01"

let peer_properties = Table
type peer_properties = table

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

  let frame          = Octet :: Short :: Longstr :: Octet :: Nil
  let scan_frame     = scan frame
  let print_frame : (string IO.output -> 'a)     = print frame

  let method_frame   = Short :: Short :: Nil
  let content_header = Short :: Short :: Longlong :: Short :: Nil

  let scan_method_frame = scan method_frame
  let print_method_frame : (string IO.output -> 'a)  = print method_frame

  let scan_content_header = scan content_header
  let print_content_header  : (string IO.output -> 'a) = print content_header

  let write output channel (hdr, message) =
    let o = IO.output_string () in
    let (data, tpe) = match message with
      | Method data -> data, Amqp_spec.frame_method
      | Body data -> data, Amqp_spec.frame_header (* TODO *)
    in
    let data = Tuple2.uncurry (sprint method_frame) hdr ^ data in
    let s = print_frame o tpe channel data 0xde in
    IO.nwrite output s;
    IO.flush output

  let read input =
    let (tpe, channel_id, data, _magic) = scan_frame (Tuple4.curry identity) input in
    Printf.printf "Channel: %d\n%!" channel_id;
    let channel = Hashtbl.find channels channel_id in
    match tpe with
    | n when n = Amqp_spec.frame_method ->
      (* Standard method message *)
      assert (channel.state = Ready);
      let hdr = scan_method_frame (Tuple2.curry identity) (IO.input_string data) in
      let data = String.slice ~first:4 data in
      channel.callback hdr (Method data)
    | n when n = Amqp_spec.frame_header ->
      assert (channel.state = Ready);
      (* Decode the header frame *)
      let cls_id, mth_id, size, _magic =
        scan_content_header
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
let handle1 (_class, _method, spec, make, _apply) (r_class, r_method, r_spec, _r_make, r_apply) =
  let scan = sscan spec in
  let print = sprint r_spec in
  fun callback buf ->
    let req = scan make buf in
    let rep = callback req in
    let data = r_apply print rep in
    ( (r_class, r_method), Framing.Method data )

module Start = struct
  let handle = handle1 Amqp_spec.Connection.Start.def Amqp_spec.Connection.Start_ok.def
end

let handle_start {Amqp_spec.Connection.Start.version_major;
                  version_minor;
                  server_properties;
                  mechanisms;
                  locales } =
  Printf.printf "Connection start\n";
  Printf.printf "Id: %d %d\n" version_major version_minor;
  (* Printf.printf "Properties: %s\n" server_properties; *)
  Printf.printf "Mechanisms: %s\n" mechanisms;
  Printf.printf "Locales: %s\n" locales;
  {
    Amqp_spec.Connection.Start_ok.client_properties = server_properties;
    mechanism = ""; response = "Sure";
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
  Framing.read t.Transport.i


(* We manually need to handle main connection status and other protocol thingys. *)

(* When the user ( application programmer ) calls a function (to send data) the user needs to register a callback handler.

Something like
val qos: { qos parameters} -> (fun {qos_response} -> ())
the unit returned could be a CSP style thing - Like a {qos_resp} Lwt.t

To retrieve messages, the user needs to register a on_deliver handler. These are just bodies.
For oob, the user need to register oob handler.

Following messages needs to handle bodies:
deliver
return
get_ok (as a reply to get) - We dont need to implement get, but we could
We know that the reply will have a body (well - may)
- We can code that manually. Also We could code everything manually.
There are not that many cases, and while coding we will see
a pattern.

Each channel has a state.
The user needs to register for OOP messages.
We can then call the callback with the correct type...
We can see if we need to implement handlers based on fixtures
(client or server). Nah...

All message marked 'server', synchronious and with reply can be called.
All messages marked 'client', synchronious with reply must have a handler that produces the required reply type (* I think we need a map to know which *).

All message marked 'client' not synchronious are oob and should have a handler that procuces no reply.

How the hell do we handle requests that can produce multiple replies?
- Try create an algebraic data structure - Or use polymorphic variants.

I think I got it now.

*)

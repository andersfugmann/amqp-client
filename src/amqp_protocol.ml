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

module Start = struct
  let spec = Octet :: Octet :: peer_properties :: Longstr :: Longstr :: Nil
  type t = { version_major: octet;
             version_minor: octet;
             server_properties: peer_properties;
             mechanisms: longstr;
             locales: longstr }
  let to_t
      version_major
      version_minor
      server_properties
      mechanisms
      locales =
    { version_major;
      version_minor;
      server_properties;
      mechanisms;
      locales;
    }

  let of_t f
      { version_major;
        version_minor;
        server_properties;
        mechanisms;
        locales;
      } = f
      version_major
      version_minor
      server_properties
      mechanisms
      locales


  let t = { Amqp_types.class_id = 10;
            method_id = 10;
            spec = ESpec spec;
            content = None }

  let print =
    let printer = sprint spec in
    of_t printer

  let scan = sscan spec to_t

end

module Start_ok = struct
  let spec = peer_properties :: Shortstr :: Longstr :: Shortstr :: Nil
  type t = { client_properties: peer_properties;
             mechanism: shortstr;
             response: longstr;
             locale: shortstr
           }

  let to_t
      client_properties
      mechanism
      response
      locale =
    { client_properties;
      mechanism;
      response;
      locale
    }

  let of_t f
      { client_properties;
        mechanism;
        response;
        locale
      } = f
      client_properties
      mechanism
      response
      locale

  let print =
    let printer = sprint spec in
    of_t printer

  let scan = sscan spec to_t


  let t = { Amqp_types.class_id = 10;
            method_id = 11;
            spec = ESpec spec;
            content = None }
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

  let frame          = Octet :: Short :: Longstr :: Octet :: Nil
  let scan_frame     = scan frame
  let print_frame : (string IO.output -> 'a)     = print frame

  let method_frame   = Short :: Short :: Nil
  let content_header = Short :: Short :: Longlong :: Short :: Nil

  let scan_method_frame = scan method_frame
  let print_method_frame : (string IO.output -> 'a)  = print method_frame

  let scan_content_header = scan content_header
  let print_content_header  : (string IO.output -> 'a) = print content_header

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

let dispatch = function
  | { Framing.class_id = 10; method_id = 10  } ->
    fun s -> let res = Start.scan s in
      dump res
  | { Framing.class_id = 10; method_id = 11  } ->
    fun s -> let res = Start_ok.scan s in
      dump res
  | _ -> failwith "Not suported"


let print_data (hdr, data) =
  let s = match data with
    | Framing.Method data -> dispatch hdr data
    | Framing.Body _data -> failwith "Body not supported"
  in
  Printf.printf "Data: (%d, %d) %s\n%!"
    hdr.Framing.class_id
    hdr.Framing.method_id
    s


let init () =
  let t = Transport.connect () in
  print_endline "Connected";
  Framing.register_callback 0 print_data;
  Framing.read_frame t.Transport.i


(* We manually need to handle main connection status and other protocol thingys. *)

(* When the user ( application programmer ) calls a function (to send data) the user needs to register a callback handler.

Something like
val qos: { qos parameters} -> (fun {qos_response} -> ())
the unit returned could be a CSP style thing - Like a {qos_resp} Lwt.t

*)

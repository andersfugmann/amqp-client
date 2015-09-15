open Batteries
open Amqp_types

exception Unknown_frame of int

let protocol_header = "AMQP\x00\x00\x09\x01"

module Transport = struct
  type t = { i: IO.input; o: unit IO.output; }

  let rec to_bits acc c = function
    | n, Type.Bit :: xs when n > 0 -> to_bits (Param.Bit (c mod 2 = 1) :: acc) (c lsr 1) ((n-1), xs)
    | _, xs -> List.rev acc, xs

  let from_bits lst =
    let rec inner c = function
      | n, Param.Bit true :: xs when n > 0 -> inner ((c + 1) lsl 1) (n-1, xs)
      | n, Param.Bit false :: xs when n > 0 -> inner (c lsl 1) (n-1, xs)
      | _, xs -> c, xs
    in
    inner 0 (8, lst)

  let rec read t = function
    | (Type.Bit :: _) as xs ->
        let r, xs = to_bits [] (IO.read_byte t) (8, xs) in
        r @ read t xs
    | Type.Octet :: xs ->
        Param.Octet (IO.read_byte t) :: read t xs
    | Type.Short :: xs ->
        Param.Short (IO.read_ui16 t) :: read t xs
    | Type.Long :: xs ->
        Param.Long (IO.read_i32 t) :: read t xs (* TODO: should be unsigned *)
    | Type.Longlong :: xs ->
        Param.Longlong (Int64.to_int (IO.read_i64 t)) :: read t xs (* TODO: should be unsigned *)
    | Type.Shortstr :: xs ->
        let len = IO.read_byte t in
        Param.Shortstr (IO.nread t len) :: read t xs
    | Type.Longstr :: xs ->
        let len = IO.read_i32 t in
        Param.Longstr (IO.nread t len) :: read t xs
    | Type.Table :: xs ->
        let len = IO.read_i32 t in
        IO.nread t len |> ignore;
        Param.Table () :: read t xs
    | Type.Timestamp :: xs ->
        Param.Timestamp (Int64.to_int (IO.read_i64 t)) :: read t xs (* TODO: should be unsigned *)
    | [] -> []

  let rec write t = function
    | (Param.Bit _ :: _) as xs ->
        let c, xs = from_bits xs in
        IO.write_byte t c;
        write t xs
    | Param.Octet n:: xs ->
        IO.write_byte t n;
        write t xs
    | Param.Short n :: xs ->
        IO.write_ui16 t n;
        write t xs
    | Param.Long n :: xs ->
        IO.write_i32 t n;
        write t xs
    | Param.Longlong n :: xs ->
        IO.write_i64 t (Int64.of_int n);
        write t xs
    | Param.Shortstr s :: xs ->
        IO.write_byte t (String.length s);
        IO.write_string t s;
        write t xs
    | Param.Longstr s :: xs ->
        IO.write_i32 t (String.length s);
        IO.write_string t s;
        write t xs
    | Param.Table () :: xs ->
        IO.write_i32 t 0;
        write t xs
    | Param.Timestamp n :: xs ->
        IO.write_i64 t (Int64.of_int n);
        write t xs
    | [] -> ()

  let connect () =
    let i, o = Unix.(open_connection ~autoclose:false (ADDR_INET (inet_addr_loopback, 5672))) in
    IO.write_string o protocol_header;
    IO.flush o;
    { i; o }

end
module Method = struct
end
(** Demultiplex into channels *)
module Framing = struct
  (* Imperative. We will change that later *)
  let channels = Hashtbl.create 0
  type hdr = { class_id: int; method_id: int }

  type channel_state =
    | Ready
    | Waiting of hdr * int * Buffer.t

  type message =
    | Method of string
    | Body of hdr * string

  type callback = message -> unit

  type channel = {  callback: callback;
                    mutable state: channel_state;
                 }

  let frame          = Type.[ Octet; Short; Longstr; Octet ]
  let method_frame   = Type.[ Short; Short ]
  let content_header = Type.[ Short; Short; Longlong; Short; ]

  let read_frame input =
    match Transport.read input frame with
    | [ Octet tpe; Octet channel; Longstr data; Octet magic ] ->
      begin
        let channel = Hashtbl.find channels channel in
        match tpe with
        | n when n = Amqp_spec.frame_method ->
          (* Standard method message *)
          assert (channel.state = Ready);
          channel.callback (Message data)
        | n when n = Amqp_spec.frame_header ->



          (* Methods which has a body (content) *)
          (* Will send a header message to indicate the amount of data in
             the body *)
        | n when n = Amqp_spec.body ->
          (* Body messages should be assembled based on the header message *)

        | n when n = Amqp_spec.heartbeat ->
          ()
        | n -> raise Unknown_frame n
      end
    | _ -> raise "Error"


  let write_channel n data = ()





let init () =
  let t = Transport.connect () in
  print_endline "Connected";
  let resp = Transport.read t.Transport.i frame in
  Printf.printf "Got response: %s\n" (dump resp);
  ()

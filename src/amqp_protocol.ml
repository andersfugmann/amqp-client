open Batteries
open Amqp_types

exception Unknown_frame of int
exception Decode_error


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
        let b = IO.read_byte t in
        Printf.printf "Octet: x%02x\n" b;
        Param.Octet b :: read t xs
    | Type.Short :: xs ->
        let s = IO.BigEndian.read_ui16 t in
        Printf.printf "Short: x%02x\n" s;
        Param.Short s :: read t xs
    | Type.Long :: xs ->
        let l = IO.BigEndian.read_i32 t in
        Printf.printf "Long: l%04x\n" l;
        Param.Long l :: read t xs (* TODO: should be unsigned *)
    | Type.Longlong :: xs ->
        let ll = IO.BigEndian.read_i64 t |> Int64.to_int in
        Printf.printf "Longlong: l%08x\n" ll;
        Param.Longlong ll :: read t xs (* TODO: should be unsigned *)
    | Type.Shortstr :: xs ->
        let len = IO.read_byte t in
        Printf.printf "Shortstr len: d%d\n" len;
        let s = IO.nread t len in
        Param.Shortstr s :: read t xs
    | Type.Longstr :: xs ->
        let len = IO.BigEndian.read_i32 t in
        Printf.printf "Longstr len: d%d\n" len;
        let s = IO.nread t len in
        Param.Longstr s :: read t xs
    | Type.Table :: xs ->
        let len = IO.BigEndian.read_i32 t in
        IO.nread t len |> ignore;
        Param.Table () :: read t xs
    | Type.Timestamp :: xs ->
        let ts = IO.BigEndian.read_i64 t |> Int64.to_int in
        Param.Timestamp ts :: read t xs (* TODO: should be unsigned *)
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
        IO.BigEndian.write_ui16 t n;
        write t xs
    | Param.Long n :: xs ->
        IO.BigEndian.write_i32 t n;
        write t xs
    | Param.Longlong n :: xs ->
        Int64.of_int n |> IO.BigEndian.write_i64 t;
        write t xs
    | Param.Shortstr s :: xs ->
        IO.write_byte t (String.length s);
        IO.write_string t s;
        write t xs
    | Param.Longstr s :: xs ->
        String.length s |> IO.BigEndian.write_i32 t;
        IO.write_string t s;
        write t xs
    | Param.Table () :: xs ->
        IO.BigEndian.write_i32 t 0;
        write t xs
    | Param.Timestamp n :: xs ->
        Int64.of_int n |> IO.BigEndian.write_i64 t;
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
(* Really need a better way of decoding.
   The spec using lists is really bad
 *)




module Framing = struct
  (* Imperative. We will change that later *)
  type hdr = { class_id: int; method_id: int }

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

  let frame          = Type.[ Octet; Short; Longstr; Octet ]
  let method_frame   = Type.[ Short; Short ]
  let content_header = Type.[ Short; Short; Longlong; Short; ]

  let decode_header data =
    let open Param in
    let input = IO.input_string data in
    match Transport.read input content_header with
    | [ Short class_id; Short method_id; Longlong size; Short _flags ] ->
      { class_id; method_id }, size
    | _ -> raise Decode_error

  let decode_method data =
    let open Param in
    let input = IO.input_string data in
    match Transport.read input method_frame with
    | [ Short class_id; Short method_id ] ->
      { class_id; method_id }
    | _ -> raise Decode_error

  let read_frame input =
    let open Param in
    match Transport.read input frame with
    | [ Octet tpe; Short channel; Longstr data; Octet _magic ] ->
      begin
        Printf.printf "Channel: %d\n" channel;
        let channel = Hashtbl.find channels channel in
        match tpe with
        | n when n = Amqp_spec.frame_method ->
          (* Standard method message *)
          assert (channel.state = Ready);
          let hdr = decode_method data in
          let data = String.slice ~first:4 data in
          channel.callback (hdr, Method data)
        | n when n = Amqp_spec.frame_header ->
          assert (channel.state = Ready);
          (* Decode the header frame *)
          let hdr, size = decode_header data in
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
      end
    | _ -> raise Decode_error

  let register_callback channel callback =
    Hashtbl.add channels channel { callback; state = Ready }

end




let print_data (hdr, data) =
  Printf.printf "Data: (%d, %d) Len: %d\n"
    hdr.Framing.class_id
    hdr.Framing.method_id
    (String.length (match data with Framing.Method d | Framing.Body d -> d))

let init () =
  let t = Transport.connect () in
  print_endline "Connected";
  (*
  let dta = IO.nread t.Transport.i 20 in
  String.explode dta
  |> List.map Char.code
  |> List.iter (Printf.printf "%02x");
  print_endline "";
*)
  Framing.register_callback 0 print_data;
  Framing.read_frame t.Transport.i

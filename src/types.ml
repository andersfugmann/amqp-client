open Batteries

type message_id = int * int

type bit = bool
and octet = int
and short = int
and long = int
and longlong = int
and shortstr = string
and longstr = string
and timestamp = int
and decimal = { digits : int; value: int }
and table = (string * value) list
and array = value list
and value =
  | VBoolean of bool
  | VShortshort of int
  | VShort of int
  | VLong of int
  | VLonglong of int
  | VShortstr of string
  | VLongstr of string
  | VFloat of float
  | VDouble of float
  | VDecimal of decimal
  | VTable of (string * value) list
  | VArray of value list
  | VTimestamp of int
  | VUnit of unit

let log fmt = Printf.ifprintf stdout (fmt ^^ "\n%!");


exception Unknown_class_id of int
exception Unknown_method_id of int


type _ elem =
  | Bit: bool elem
  | Boolean: bool elem
  | Octet: int elem
  | Shortshort: int elem
  | Short: int elem
  | Long: int elem
  | Longlong: int elem
  | Shortstr: string elem
  | Longstr: string elem
  | Float: float elem
  | Double: float elem
  | Decimal: decimal elem
  | Table: table elem
  | Timestamp: timestamp elem
  | Array: array elem
  | Unit: unit elem

type (_, _) spec =
  | Nil : ('a, 'a) spec
  | ::  : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

let rec read_while t f =
  match (try Some (f t) with e -> log "%s" (Printexc.to_string e); None) with
  | Some v -> log "Field"; List.cons v (read_while t f)
  | None -> []


let rec decode: type a. a elem -> IO.input -> a = fun elem t ->
  match elem with
  | Bit -> IO.read_byte t = 1 |> tap (log "Bit %b")
  | Octet -> IO.read_byte t |> tap (log "Octet 0x%02x")
  | Short -> IO.BigEndian.read_ui16 t |> tap (log "Short 0x%04x")
  | Long -> IO.BigEndian.read_i32 t |> tap (log "Long 0x%08x")
  | Longlong -> IO.BigEndian.read_i64 t |> Int64.to_int |> tap (log "Longlong 0x%16x")
  | Shortstr ->
    let len = decode Octet t in
    IO.really_nread t len
  | Longstr ->
    let len = decode Long t in
    IO.really_nread t len
  | Table ->
    let s = decode Longstr t in
    let t = IO.input_string s in
    let read_table_value t =
      let key = decode Shortstr t in
      let value = decode_field t in
      (key, value)
    in
    read_while t read_table_value
  | Timestamp -> decode Longlong t
  | Boolean -> decode Octet t = 1
  | Shortshort -> decode Octet t
  | Float -> IO.BigEndian.read_float t
  | Double -> IO.BigEndian.read_double t
  | Decimal ->
    let digits = decode Octet t in
    let value = decode Long t in
    { digits; value }
  | Array ->
    let data = decode Longstr t in
    let is = IO.input_string data in
    read_while is decode_field
  | Unit -> ()
and decode_field t =
  match IO.read t with
  | 't' -> VBoolean (decode Boolean t)
  | 'b' | 'B' -> VShortshort (decode Shortshort t)
  | 'u' | 'U' -> VShort (decode Short t)
  | 'i' | 'I' -> VLong (decode Long t)
  | 'l' | 'L' -> VLonglong (decode Longlong t)
  | 'f' -> VFloat (decode Float t)
  | 'd' -> VDouble (decode Double t)
  | 'D' -> VDecimal (decode Decimal t)
  | 's' -> VShortstr (decode Shortstr t)
  | 'S' -> VLongstr (decode Longstr t)
  | 'A' -> VArray (decode Array t)
  | 'T' -> VTimestamp (decode Timestamp t)
  | 'F' -> VTable (decode Table t)
  | 'V' -> VUnit (decode Unit t)
  | _ -> failwith "Uknown table value"

let rec encode: type a b. a elem -> b IO.output -> a -> unit = function
  | Bit -> fun t x -> IO.write_byte t (if x then 1 else 0)
  | Octet -> IO.write_byte
  | Short -> IO.BigEndian.write_ui16
  | Long -> IO.BigEndian.write_i32
  | Longlong -> fun t x -> Int64.of_int x |> IO.BigEndian.write_i64 t
  | Shortstr ->
    let enc = encode Octet in
    fun t x ->
      enc t (String.length x);
      IO.really_output t x 0 (String.length x) |> ignore
  | Longstr ->
    let enc = encode Long in
    fun t x ->
      enc t (String.length x);
      IO.really_output t x 0 (String.length x) |> ignore
  | Table -> fun t x ->
    let os = IO.output_string () in
    List.iter (fun (k, v) ->
        encode Shortstr os k;
        encode_field os v
      ) x;
    IO.close_out os
    |> encode Longstr t
  | Timestamp ->
    encode Longlong
  | Boolean ->
    let enc = encode Octet in
    fun t x -> enc t (if x then 1 else 0)
  | Shortshort ->
    encode Octet
  | Float -> IO.BigEndian.write_float
  | Double -> IO.BigEndian.write_double
  | Decimal ->
    let denc = encode Octet in
    let venc = encode Long in
    fun t { digits; value } ->
      denc t digits;
      venc t value;
  | Array -> fun t x ->
    let os = IO.output_string () in
    List.iter (encode_field os) x;
    encode Longstr t (IO.close_out os)
  | Unit -> fun _ _ -> ()
and encode_field t = function
  | VBoolean b ->
    encode Octet t (Char.code 't');
    encode Boolean t b
  | VShortshort i ->
    encode Octet t (Char.code 'b');
    encode Shortshort t i
  | VShort i ->
    encode Octet t (Char.code 'u');
    encode Short t i
  | VLong i ->
    encode Octet t (Char.code 'i');
    encode Long t i
  | VLonglong i ->
    encode Octet t (Char.code 'l');
    encode Longlong t i
  | VShortstr s ->
    encode Octet t (Char.code 's');
    encode Shortstr t s
  | VLongstr v ->
    encode Octet t (Char.code 'S');
    encode Longstr t v
  | VFloat v ->
    encode Octet t (Char.code 'f');
    encode Float t v
  | VDouble v ->
    encode Octet t (Char.code 'd');
    encode Double t v
  | VDecimal v ->
    encode Octet t (Char.code 'D');
    encode Decimal t v
  | VTable v ->
    encode Octet t (Char.code 'F');
    encode Table t v
  | VArray a ->
    encode Octet t (Char.code 'A');
    encode Array t a
  | VTimestamp v ->
    encode Octet t (Char.code 'T');
    encode Timestamp t v
  | VUnit () ->
    encode Octet t (Char.code 'V');
    encode Unit t ()

let rec read: type b c. (b, c) spec -> b -> IO.input -> c = function
  | (Bit :: _) as spec ->
    let reader = read_bits 8 spec
    and decoder = decode Octet in
    fun b t -> reader b (decoder t) t
  | head :: tail ->
    let reader = read tail
    and decoder = decode head in
    fun b t -> reader (b (decoder t)) t
  | Nil ->
    fun b _t -> b
and read_bits: type b c. int -> (b, c) spec -> b -> int -> IO.input -> c = fun c -> function
  | Bit :: tail when c > 0 ->
    let reader = read_bits (c - 1) tail in
    fun b v t -> reader (b (v mod 2 = 1)) (v/2) t
  | spec ->
    let reader = read spec in
    fun b _v t -> reader b t


let rec write: type b c. (b, c IO.output) spec -> c IO.output -> b = function
  | (Bit :: _) as spec ->
    let writer = write_bits 8 spec in
    fun t -> writer t 0
  | spec :: tail ->
    let encoder = encode spec
    and writer = write tail in
    fun t x -> encoder t x; writer t
  | Nil -> identity
and write_bits: type b c. int -> (b, c IO.output) spec -> c IO.output -> int -> b = fun c -> function
  | Bit :: tail when c > 0 ->
    let writer = write_bits (c-1) tail in
    fun t v x -> writer t (v*2 + (if x then 1 else 0))
  | spec ->
    let encoder = encode Octet
    and writer = write spec in
    fun t v -> encoder t v; writer t

let elem_to_string: type a. a elem -> string = function
  | Bit -> "Bit"
  | Octet -> "Octet"
  | Short -> "Short"
  | Long -> "Long"
  | Longlong -> "Longlong"
  | Shortstr -> "Shortstr"
  | Longstr -> "Longstr"
  | Table -> "Table"
  | Timestamp -> "Timestamp"
  | _ -> "Unknown"

let rec to_string: type a b. (a, b) spec -> string = function
  | x :: xs -> elem_to_string x ^ " :: " ^ to_string xs
  | Nil -> "Nil"

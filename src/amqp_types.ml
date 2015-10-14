open Batteries
open Amqp_protocol

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
  | VTable of table
  | VArray of value list
  | VTimestamp of int
  | VUnit of unit

let log fmt = Printf.ifprintf stdout (fmt ^^ "\n%!")


exception Unknown_class_id of int
exception Unknown_method_id of int


type _ elem =
  | Bit: bool elem
  | Octet: int elem
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

let reserved_value: type a. a elem -> a = function
  | Bit -> false
  | Octet -> 0
  | Short -> 0
  | Long -> 0
  | Longlong -> 0
  | Shortstr -> ""
  | Longstr -> ""
  | Float -> 0.0
  | Double -> 0.0
  | Decimal -> { digits = 0; value = 0 }
  | Table -> []
  | Timestamp -> 0
  | Array -> []
  | Unit -> ()

module Spec = struct
  type (_, _) spec =
    | Nil : ('a, 'a) spec
    | ::  : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec
end

let rec read_while t f =
  match (try Some (f t) with e -> log "%s" (Printexc.to_string e); None) with
  | Some v -> log "Field"; v :: (read_while t f)
  | None -> []

let rec decode: type a. a elem -> Input.t -> a = fun elem t ->
  match elem with
  | Bit -> Input.octet t = 1 |> tap (log "Bit %b")
  | Octet -> Input.octet t |> tap (log "Octet 0x%02x")
  | Short -> Input.short t |> tap (log "Short 0x%04x")
  | Long -> Input.long t |> tap (log "Long 0x%08x")
  | Longlong -> Input.longlong t |> tap (log "Longlong 0x%16x")
  | Shortstr ->
    let len = decode Octet t in
    Input.string t len
  | Longstr ->
    let len = decode Long t in
    Input.string t len
  | Table ->
    let s = decode Longstr t in
    let t = Input.create s in
    let read_table_value t =
      let key = decode Shortstr t in
      let value = decode_field t in
      (key, value)
    in
    read_while t read_table_value
  | Timestamp -> decode Longlong t
  | Float -> Input.float t
  | Double -> Input.double t
  | Decimal ->
    let digits = decode Octet t in
    let value = decode Long t in
    { digits; value }
  | Array ->
    let data = decode Longstr t in
    let is = Input.create data in
    read_while is decode_field
  | Unit -> ()
and decode_field t =
  match Input.octet t |> Char.chr with
  | 't' -> VBoolean (decode Bit t)
  | 'b' | 'B' -> VShortshort (decode Octet t)
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


let rec encode: type a. a elem -> Output.t -> a -> unit = function
  | Bit -> fun t x -> Output.octet t (if x then 1 else 0)
  | Octet -> Output.octet
  | Short -> Output.short
  | Long -> Output.long
  | Longlong -> Output.longlong
  | Shortstr ->
    let enc = encode Octet in
    fun t x ->
      enc t (String.length x);
      Output.string t x
  | Longstr ->
    let enc = encode Long in
    fun t x ->
      enc t (String.length x);
      Output.string t x
  | Table -> fun t x ->
    let size_ref = Output.size_ref t in
    List.iter (fun (k, v) ->
        encode Shortstr t k;
        encode_field t v
      ) x;
    size_ref ()
  | Timestamp ->
    encode Longlong
  | Float -> Output.float
  | Double -> Output.double
  | Decimal ->
    let denc = encode Octet in
    let venc = encode Long in
    fun t { digits; value } ->
      denc t digits;
      venc t value;
  | Array -> fun t x ->
    let size_ref = Output.size_ref t in
    List.iter (encode_field t) x;
    size_ref ()
  | Unit -> fun _ _ -> ()
and encode_field t = function
  | VBoolean b ->
    encode Octet t (Char.code 't');
    encode Bit t b
  | VShortshort i ->
    encode Octet t (Char.code 'b');
    encode Octet t i
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

open Spec
let rec read: type b c. (b, c) spec -> b -> Input.t -> c = function
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
and read_bits: type b c. int -> (b, c) spec -> b -> int -> Input.t -> c = fun c -> function
  | Bit :: tail when c > 0 ->
    let reader = read_bits (c - 1) tail in
    fun b v t -> reader (b (v mod 2 = 1)) (v/2) t
  | spec ->
    let reader = read spec in
    fun b _v t -> reader b t


let rec write: type b. (b, Output.t) spec -> Output.t -> b = function
  | (Bit :: _) as spec ->
    write_bits 8 spec 0
  | spec :: tail ->
    let encoder = encode spec
    and writer = write tail in
    fun t x -> encoder t x; writer t
  | Nil -> identity
and write_bits: type b. int -> (b, Output.t) spec -> int -> Output.t -> b = fun c -> function
  | Bit :: tail when c > 0 ->
    let writer = write_bits (c-1) tail in
    fun v t x -> writer (v*2 + (if x then 1 else 0)) t
  | spec ->
    let encoder = encode Octet
    and writer = write spec in
    fun v t -> encoder t v; writer t

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

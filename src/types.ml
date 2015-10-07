open Batteries
open Protocol

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

let log fmt = Printf.ifprintf stdout (fmt ^^ "\n%!")


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

let rec length: type a. a elem -> a -> int = function
  | Boolean -> const 1
  | Bit -> const 1
  | Octet -> const 1
  | Short -> const 2
  | Shortshort -> const 2
  | Long -> const 4
  | Longlong -> const 8
  | Shortstr ->
    fun v ->
      let len = String.length v in
      length Octet len + len
  | Longstr -> fun v ->
    let len = String.length v in
    length Long len + len
  | Table -> fun t ->
    let len = table_length t in
    length Long len + len
  | Array -> fun t ->
    let len = array_length t in
    length Long len + len
  | Timestamp -> length Longlong
  | Float -> const 4
  | Double -> const 8
  | Decimal -> const (1 + 8)
  | Unit -> const 0
and table_length t =
  List.fold_left (fun acc (k,v) -> acc + length Shortstr k + field_length v) 0 t
and array_length t = List.fold_left  (fun acc v -> acc + field_length v) 0 t
and field_length = function
  | VBoolean v    -> 1 + length Boolean v
  | VShortshort v -> 1 + length Shortshort v
  | VShort v      -> 1 + length Short v
  | VLong v       -> 1 + length Long v
  | VLonglong v   -> 1 + length Longlong v
  | VShortstr v   -> 1 + length Shortstr v
  | VLongstr v    -> 1 + length Longstr v
  | VFloat v      -> 1 + length Float v
  | VDouble v     -> 1 + length Double v
  | VDecimal v    -> 1 + length Decimal v
  | VTable v      -> 1 + length Table v
  | VArray v      -> 1 + length Array v
  | VTimestamp v  -> 1 + length Timestamp v
  | VUnit v       -> 1 + length Unit v

let rec read_while t f =
  match (try Some (f t) with e -> log "%s" (Printexc.to_string e); None) with
  | Some v -> log "Field"; List.cons v (read_while t f)
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
  | Boolean -> decode Octet t = 1
  | Shortshort -> decode Octet t
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
    Output.long t (table_length x);
    List.iter (fun (k, v) ->
        encode Shortstr t k;
        encode_field t v
      ) x;
  | Timestamp ->
    encode Longlong
  | Boolean ->
    let enc = encode Octet in
    fun t x -> enc t (if x then 1 else 0)
  | Shortshort ->
    encode Octet
  | Float -> Output.float
  | Double -> Output.double
  | Decimal ->
    let denc = encode Octet in
    let venc = encode Long in
    fun t { digits; value } ->
      denc t digits;
      venc t value;
  | Array -> fun t x ->
    Output.long t (array_length x);
    List.iter (encode_field t) x;
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

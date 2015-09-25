open Batteries

type bit = bool
type octet = int
type short = int
type long = int
type longlong = int
type shortstr = string
type longstr = string
type table = unit
type timestamp = int

let log fmt = Printf.ifprintf stdout fmt

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
  | Table: table elem
  | Timestamp: timestamp elem

type (_, _) spec =
  | Nil : ('a, 'a) spec
  | ::  : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

let rec decode: type a. a elem -> IO.input -> a = function
  | Bit -> fun t -> IO.read_byte t = 1 |> tap (log "Bit %b\n%!")
  | Octet -> fun t -> IO.read_byte t |> tap (log "Octet 0x%02x\n%!")
  | Short -> fun t -> IO.BigEndian.read_ui16 t |> tap (log "Short 0x%04x\n%!")
  | Long -> fun t -> IO.BigEndian.read_i32 t |> tap (log "Long 0x%08x\n%!")
  | Longlong -> fun t -> IO.BigEndian.read_i64 t |> Int64.to_int |> tap (log "Longlong 0x%16x\n%!")
  | Shortstr -> fun t ->
    let len = decode Short t in
    IO.nread t len
  | Longstr -> fun t ->
    let len = decode Long t in
    IO.nread t len
  | Table -> fun t ->
    let len = decode Long t in
    IO.nread t len |> ignore
  | Timestamp -> fun t -> decode Longlong t

let rec encode: type a b. a elem -> b IO.output -> a -> unit = function
  | Bit -> fun t x -> IO.write_byte t (if x then 1 else 0)
  | Octet -> IO.write_byte
  | Short -> IO.BigEndian.write_ui16
  | Long -> IO.BigEndian.write_i32
  | Longlong -> fun t x -> Int64.of_int x |> IO.BigEndian.write_i64 t
  | Shortstr -> let enc = encode Short in
    fun t x ->
      enc t (String.length x);
      IO.nwrite t x
  | Longstr -> let enc = encode Long in
    fun t x ->
      enc t (String.length x);
      IO.nwrite t x
  | Table -> let enc = encode Long in
    fun t _ ->
      enc t 0;
  | Timestamp -> encode Longlong


let rec elem_size: type a. a elem -> int = function
  | Bit -> 1
  | Octet -> 1
  | Short -> 2
  | Long -> 4
  | Longlong -> 8
  | Shortstr -> elem_size Short
  | Longstr -> elem_size Long
  | Table -> elem_size Longstr
  | Timestamp -> elem_size Longlong

let rec spec_min_len: type b c. (b, c) spec -> int = function
  | head :: tail -> elem_size head + spec_min_len tail
  | Nil -> 0

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

let rec to_string: type a b. (a, b) spec -> string = function
  | x :: xs -> elem_to_string x ^ " :: " ^ to_string xs
  | Nil -> "Nil"

type espec = ESpec: ('a, 'b) spec -> espec

type message = { class_id: int; method_id: int; spec: espec; content: espec option;}

(* Need functions - not specs *)

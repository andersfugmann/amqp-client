open Batteries

type bit = bool
type octet = int
type short = int
type long = int
type longlong = int
type shortstr = string
type longstr = string
(* type routing_key = string *)
type table = unit
type timestamp = int

exception Unknown_major of int
exception Unknown_minor of int

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
  | Nil: ('a, 'a) spec
  | Cons: 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

let rec decode: type a. a elem -> IO.input -> a = function
  | Bit -> fun t -> IO.read_byte t = 1 |> tap (Printf.printf "Bit %b\n%!")
  | Octet -> fun t -> IO.read_byte t |> tap (Printf.printf "Octet 0x%02x\n%!")
  | Short -> fun t -> IO.BigEndian.read_ui16 t |> tap (Printf.printf "Short 0x%04x\n%!")
  | Long -> fun t -> IO.BigEndian.read_i32 t |> tap (Printf.printf "Long 0x%08x\n%!")
  | Longlong -> fun t -> IO.BigEndian.read_i64 t |> Int64.to_int |> tap (Printf.printf "Longlong 0x%16x\n%!")
  | Shortstr -> fun t ->
    let len = decode Short t in
    IO.nread t len
  | Longstr -> fun t ->
    let len = decode Long t in
    IO.nread t len
  | Table ->  fun t ->
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
  | Cons(head, tail) -> elem_size head + spec_min_len tail
  | Nil -> 0

let rec scan: type b c. (b, c) spec -> IO.input -> b -> c = function
  | Cons(Bit, _) as tail -> fun t b -> scan_bits 8 tail t b (decode Octet t)
  | Cons (head, tail) ->
    let scanner = scan tail
    and decoder = decode head in
    fun t b -> scanner t (b (decoder t))
  | Nil -> fun _t b -> b
and scan_bits: type b c. int -> (b, c) spec -> IO.input -> b -> int -> c = fun c -> function
  | Cons(Bit, tail) when c > 0 ->
    let scanner = scan_bits (c - 1) tail in
    fun t b v -> scanner t (b (v mod 2 = 1)) (v/2)
  | tail ->
    let scanner = scan tail in
    fun t b _v -> scanner t b

let rec print: type b c. (b, c) spec -> c IO.output -> b = function
  | Cons (Bit, _) as tail ->
    let printer = print_bits 8 tail in
    fun t -> printer t 0
  | Cons (head, tail) ->
    let encoder = encode head
    and printer = print tail in
    fun t x -> encoder t x; printer t
  | Nil -> fun t -> IO.close_out t
and print_bits: type b c. int -> (b, c) spec -> c IO.output -> int -> b = fun c -> function
  | Cons(Bit, tail) when c > 0 ->
    let printer = print_bits (c-1) tail in
    fun t v x -> printer t (v*2 + (if x then 1 else 0))
  | tail ->
    let encoder = encode Octet
    and printer = print tail in
    fun t v -> encoder t v; printer t




(* Helper function for spec construction *)
let (@) elem tail = Cons (elem, tail)
let eol = Nil

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
  | Cons(x, xs) -> elem_to_string x ^ " @ " ^ to_string xs
  | Nil -> "eol"

type espec = ESpec: ('a, 'b) spec -> espec

type message = { class_id: int; method_id: int; spec: espec; content: espec option }

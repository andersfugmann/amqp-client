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

let rec decode: type a. IO.input -> a elem -> a = fun t -> function
  | Bit -> IO.read_byte t = 1 |> tap (Printf.printf "Bit %b\n%!")
  | Octet -> IO.read_byte t |> tap (Printf.printf "Octet 0x%02x\n%!")
  | Short -> IO.BigEndian.read_ui16 t |> tap (Printf.printf "Short 0x%04x\n%!")
  | Long -> IO.BigEndian.read_i32 t |> tap (Printf.printf "Long 0x%08x\n%!")
  | Longlong -> IO.BigEndian.read_i64 t |> Int64.to_int |> tap (Printf.printf "Longlong 0x%16x\n%!")
  | Shortstr ->
    let len = decode t Short in
    IO.nread t len
  | Longstr ->
    let len = decode t Long in
    IO.nread t len
  | Table ->
    let len = decode t Long in
    IO.nread t len |> ignore
  | Timestamp -> decode t Longlong

let rec encode: type a b. b IO.output -> a elem -> a -> unit = fun t -> function
  | Bit -> fun x -> IO.write_byte t (if x then 1 else 0)
  | Octet -> fun x -> IO.write_byte t x
  | Short -> fun x -> IO.BigEndian.write_ui16 t x
  | Long -> fun x -> IO.BigEndian.write_i32 t x
  | Longlong -> fun x -> Int64.of_int x |> IO.BigEndian.write_i64 t
  | Shortstr -> fun x ->
    encode t Short (String.length x);
    IO.nwrite t x
  | Longstr -> fun x ->
    encode t Long (String.length x);
    IO.nwrite t x
  | Table -> fun () ->
    encode t Long 0;
  | Timestamp -> encode t Longlong

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

let rec scan: type b c. IO.input -> b -> (b, c) spec -> c = fun t b -> function
  | Cons(Bit, _) as tail -> scan_bits t b (decode t Octet) 8 tail
  | Cons (head, tail) -> scan t (b (decode t head)) tail
  | Nil -> b
and scan_bits: type b c. IO.input -> b -> int -> int -> (b, c) spec -> c = fun t b v c -> function
  | Cons(Bit, tail) when c > 0 -> scan_bits t (b (v mod 2 = 1)) (v/2) (c - 1) tail
  | tail -> scan t b tail

let rec print: type b c. c IO.output -> (b, c) spec -> b = fun t -> function
  | Cons (Bit, _) as tail -> print_bits t 0 8 tail
  | Cons (head, tail) -> fun x -> encode t head x; print t tail
  | Nil -> IO.close_out t
and print_bits: type b c. c IO.output -> int -> int -> (b, c) spec -> b = fun t v c -> function
  | Cons(Bit, tail) when c > 0 -> fun x -> print_bits t (v*2 + (if x then 1 else 0)) (c-1) tail
  | tail -> encode t Octet v; print t tail

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

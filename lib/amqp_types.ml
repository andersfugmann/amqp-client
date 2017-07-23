(** Basic Amqp types *)
exception Connection_closed
exception Channel_closed of int
exception Channel_not_found of int
exception Unknown_frame_type of int
exception No_handler_found
exception Busy

type class_id = int
type method_id = int
type message_id = class_id * method_id

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
  | VShortstr of string (* Not accepted by rabbitmq *)
  | VLongstr of string
  | VFloat of float
  | VDouble of float
  | VDecimal of decimal
  | VTable of table
  | VArray of value list
  | VTimestamp of int
  | VUnit of unit

type header = string * value

let rec print_type indent t =
  let open Printf in
  match t with
  | VTable t ->
    let indent' = indent ^ "  " in
    printf "[\n";
    List.iter (fun (k, v) -> printf "%s%s: " indent' k; print_type (indent')  v; printf "\n") t;
    printf "%s]" indent;
  | VBoolean v -> printf "%b" v
  | VShortshort v
  | VShort v
  | VLong v
  | VTimestamp v
  | VLonglong v -> printf "%d" v
  | VShortstr v
  | VLongstr v -> printf "%s" v
  | VFloat v
  | VDouble v-> printf "%f" v
  | VDecimal v -> printf "%f" (float v.value /. float v.digits)
  | VArray a ->
    let indent' = indent ^ "  " in
    printf "[\n";
    List.iter (fun v -> printf "%s" indent'; print_type (indent')  v; printf "\n") a;
    printf "%s]" indent;
  | VUnit _ -> printf "\n"

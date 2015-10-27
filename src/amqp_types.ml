(** Basic Amqp types *)

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

exception Unknown_class_id of int
exception Unknown_method_id of int

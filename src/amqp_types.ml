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

module Param = struct
  type t =
    | Bit of bit
    | Octet of octet
    | Short of short
    | Long of long
    | Longlong of longlong
    | Shortstr of shortstr
    | Longstr of longstr
    (*     | Routing_key of routing_key *)
    | Table of table
    | Timestamp of timestamp

end

module Type = struct
  type t =
    | Bit
    | Octet
    | Short
    | Long
    | Longlong
    | Shortstr
    | Longstr
    (*     | Routing_key *)
    | Table
    | Timestamp
end

type message = { major: int; minor:int; params: Param.t list}

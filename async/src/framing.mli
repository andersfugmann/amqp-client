(** Internal *)
open Thread
open Amqp_client_lib
type channel_no = int


type message = Method of Types.message_id * Io.Input.t
             | Content of Types.class_id * Io.Input.t * string

type data = Io.Input.t
type content_handler = data * string -> unit
type method_handler = data -> unit

type close_handler = string -> unit Deferred.t
type t

val write_message : t * channel_no ->
  Types.message_id * (Io.Output.t -> Io.Output.t) ->
  (Types.class_id * (Io.Output.t -> Io.Output.t) * string) option ->
  unit Deferred.t

val send_heartbeat: t -> unit Deferred.t

val register_method_handler : t * channel_no -> Types.message_id -> method_handler -> unit
val register_content_handler : t * channel_no -> Types.class_id -> content_handler -> unit
val deregister_method_handler : t * channel_no -> Types.message_id -> unit
val deregister_content_handler : t * channel_no -> Types.class_id -> unit

val set_flow : t -> channel_no -> bool -> unit
val set_flow_all : t -> bool -> unit


val open_channel : t -> channel_no -> unit Deferred.t
val close_channel : t -> channel_no -> unit Deferred.t

val flush_channel : t -> channel_no -> unit Deferred.t
val flush : t -> unit Deferred.t

val id : t -> string

val init : id:string -> Reader.t -> Writer.t -> t
val start: t -> close_handler -> unit Deferred.t
val close : t -> unit Deferred.t

val set_max_length : t -> int -> unit

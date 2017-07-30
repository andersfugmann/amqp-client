(** Internal *)
open Amqp_thread
open Amqp_types

type channel_no = int

type message = Method of Amqp_types.message_id * Amqp_io.Input.t
             | Content of Amqp_types.class_id * Amqp_io.Input.t * string

type data = Amqp_io.Input.t
type content_handler = data * string -> unit
type method_handler = data -> unit

type close_handler = string -> unit Deferred.t
type t

val write_message : t * channel_no ->
  message_id * (Amqp_io.Output.t -> Amqp_io.Output.t) ->
  (class_id * (Amqp_io.Output.t -> Amqp_io.Output.t) * string) option ->
  unit Deferred.t

val send_heartbeat: t -> unit Deferred.t

val register_method_handler : t * channel_no -> message_id -> method_handler -> unit
val register_content_handler : t * channel_no -> class_id -> content_handler -> unit
val deregister_method_handler : t * channel_no -> message_id -> unit
val deregister_content_handler : t * channel_no -> class_id -> unit

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

exception Unknown_frame_type of int
exception Connection_closed
exception Busy
exception Unhandled_method of Amqp_types.message_id
exception Unhandled_header of Amqp_types.class_id

type channel_no = int

type message =
    Method of Amqp_types.message_id * Amqp_protocol.Input.t
  | Content of Amqp_types.class_id * Amqp_protocol.Input.t * string

type data = Amqp_protocol.Input.t
type content_handler = data * string -> unit
type method_handler = data -> unit

type t

val write_method :
  t * channel_no -> int * int -> Amqp_protocol.Output.t -> unit

val write_content :
  t * channel_no -> int -> Amqp_protocol.Output.t -> string -> unit

val register_method_handler :
  t * channel_no -> Amqp_types.message_id -> method_handler -> unit
val register_content_handler :
  t * channel_no -> Amqp_types.class_id -> content_handler -> unit
val deregister_method_handler :
  t * channel_no -> Amqp_types.message_id -> unit
val deregister_content_handler :
  t * channel_no -> Amqp_types.class_id -> unit

val open_channel : t -> channel_no -> unit Async_kernel.Deferred.t

val close_channel : t -> channel_no -> unit

val flush : t -> unit Async_unix.Import.Deferred.t

val id : t -> string

val init : id:string -> port:int -> string -> t Async.Std.Deferred.t

val set_max_length : t -> int -> unit

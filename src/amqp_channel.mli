type message =
    Amqp_spec.Basic.Deliver.t * (Amqp_spec.Basic.Content.t * string)

type consumers = (string, message -> unit) Hashtbl.t

type t

val channel : t -> Amqp_framing.t * int

val register_deliver_handler : t -> unit
val register_consumer_handler : t -> string -> (message -> unit) -> unit
val deregister_consumer_handler : t -> string -> unit

val init :
  id:string ->
  Amqp_framing.t -> Amqp_framing.channel_no -> t Async.Std.Deferred.t
val close : t -> unit Async.Std.Deferred.t

(* TODO: Rename to something else *)
val on_return :
  t ->
  (Amqp_spec.Basic.Return.t * (Amqp_spec.Basic.Content.t * string) ->
   unit Async.Std.Deferred.t) ->
  unit


val next_counter : t -> int

val id : t -> string

val channel_no : t -> int

val unique_id : t -> string

val set_prefetch :
  ?count:Amqp_types.short ->
  ?size:Amqp_types.long ->
  ?global:Amqp_types.bit -> t -> unit Async_kernel.Deferred0.t

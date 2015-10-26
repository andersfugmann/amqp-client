(** Openration on channels *)
open Async.Std

type message =
    Amqp_spec.Basic.Deliver.t * (Amqp_spec.Basic.Content.t * string)

type consumers = (string, message -> unit) Hashtbl.t
type close_handler = int -> Amqp_spec.Channel.Close.t -> unit Deferred.t

type t

val channel : t -> Amqp_framing.t * int

(** Register handlers - For internal use *)

val register_deliver_handler : t -> unit
val register_consumer_handler : t -> string -> (message -> unit) -> unit
val deregister_consumer_handler : t -> string -> unit

(** Create a new channel. Use Connection.open_channel instead *)
val create :
  id:string ->
  Amqp_framing.t -> Amqp_framing.channel_no -> t Deferred.t

(** Close the channel *)
val close : t -> unit Deferred.t

(** Register a handler if the connection closes unexpectedly.
    This handler will not be called if the channel is closed by the user.
    The default handler is to terminate the application
*)
val register_close_handler: t -> close_handler -> unit

(** Register handler if messages are rejected by the amqp server. *)
val on_return :
  t ->
  (Amqp_spec.Basic.Return.t * (Amqp_spec.Basic.Content.t * string) ->
   unit Async.Std.Deferred.t) ->
  unit

(** Get the id of the channel *)
val id : t -> string

(** Get the channel_no of the connection *)
val channel_no : t -> int

(** Construct a unique id for this channel *)
val unique_id : t -> string

(** Set prefetch counters for a channel or globally (across all channels).

    Note if using rabbitmq, prefetch only affects consumers on the channel;
    If global is [true] then the pretch limit is applied across all consumers on the channel,
    othervice the prefecth limit is per consumer (on the channel).

*)
val set_prefetch :
  ?count:Amqp_types.short ->
  ?size:Amqp_types.long ->
  ?global:Amqp_types.bit -> t -> unit Async_kernel.Deferred0.t

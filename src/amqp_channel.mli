(** Operations on channels *)
open Async.Std
open Amqp_spec

(**/**)
type consumer = Basic.Deliver.t * Basic.Content.t * string -> unit
type consumers = (string, consumer) Hashtbl.t
(**/**)

type close_handler = int -> Channel.Close.t -> unit Deferred.t

type _ t

(**/**)
val channel : _ t -> Amqp_framing.t * int

module Internal : sig
  type e = E: _ t -> e
  val register_consumer_handler : _ t -> string -> consumer -> unit
  val deregister_consumer_handler : _ t -> string -> unit
  val wait_for_confirm : 'a t -> 'a Deferred.t
  val unique_id : _ t -> string
end
(**/**)

type 'a confirms

type no_confirm = [ `Ok ]
type with_confirm = [ `Ok | `Failed ]

val no_confirm: no_confirm confirms
val with_confirm: with_confirm confirms

(** Create a new channel.
    Use Connection.open_channel rather than this method directly *)
val create : id:string -> 'a confirms ->
  Amqp_framing.t -> Amqp_framing.channel_no -> 'a t Deferred.t

(** Close the channel *)
val close : _ t -> unit Deferred.t

(** Register a handler if the connection closes unexpectedly.
    This handler will not be called if the channel is closed by the user.
    The default handler is to terminate the application
*)
val register_close_handler: _ t -> close_handler -> unit

(** Register handler if messages are rejected by the amqp server. *)
val on_return :
  _ t ->
  (Basic.Return.t * (Basic.Content.t * string) ->
   unit Deferred.t) ->
  unit

(** Get the id of the channel *)
val id : _ t -> string

(** Get the channel_no of the connection *)
val channel_no : _ t -> int

(** Set prefetch counters for a channel.
    @param count: Maximum messages inflight (un-acked)
    @param site: Maximum amount of bytes inflight

    Note: if using rabbitmq, the prefetch limits are set per consumer on the channel,
    rather than per channel (across consumers)
*)
val set_prefetch : ?count:int -> ?size:int -> _ t -> unit Deferred.t

(** Set global prefetch counters.
    @param count: Maximum messages inflight (un-acked)
    @param site: Maximum amount of bytes inflight

    Note: if using rabbitmq, the prefetch limits are set per channel (across consumers),
    If not, the global prefetch settings is applied globally - across consumers and channels.
*)
val set_global_prefetch : ?count:int -> ?size:int -> _ t -> unit Deferred.t

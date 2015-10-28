(** Operations on channels *)
open Async.Std
open Amqp_spec

(**/**)
type consumer = Basic.Deliver.t * Basic.Content.t * string -> unit
type consumers = (string, consumer) Hashtbl.t
(**/**)

type close_handler = int -> Channel.Close.t -> unit Deferred.t

type t

(**/**)
val channel : t -> Amqp_framing.t * int

module Internal : sig
  val register_deliver_handler : t -> unit
  val register_consumer_handler : t -> string -> consumer -> unit
  val deregister_consumer_handler : t -> string -> unit

  (** Construct a unique id for this channel *)
  val unique_id : t -> string
end
(**/**)

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
  (Basic.Return.t * (Basic.Content.t * string) ->
   unit Deferred.t) ->
  unit

(** Get the id of the channel *)
val id : t -> string

(** Get the channel_no of the connection *)
val channel_no : t -> int

(** Set prefetch counters for a channel.
    @param count: Maximum messages inflight (un-acked)
    @param site: Maximum amount of bytes inflight

    Note: if using rabbitmq, the prefetch limits are set per consumer on the channel,
    rather than per channel (across consumers)
*)
val set_prefetch : ?count:int -> ?size:int -> t -> unit Deferred.t

(** Set global prefetch counters.
    @param count: Maximum messages inflight (un-acked)
    @param site: Maximum amount of bytes inflight

    Note: if using rabbitmq, the prefetch limits are set per channel (across consumers),
    If not, the global prefetch settings is applied globally - across consumers and channels.
*)
val set_global_prefetch : ?count:int -> ?size:int -> t -> unit Deferred.t

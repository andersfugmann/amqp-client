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
val register_close_handler : _ t -> close_handler -> unit

(** Receive all returned messages.
    This function may only be called once (per channel)
*)
val on_return : _ t ->
  (Basic.Return.t * (Basic.Content.t * string)) Pipe.Reader.t

(** Get the id of the channel *)
val id : _ t -> string

(** Get the channel_no of the connection *)
val channel_no : _ t -> int

val set_prefetch : ?count:int -> ?size:int -> _ t -> unit Deferred.t
(** Set prefetch counters for a channel.
    @param count Maximum messages inflight (un-acked)
    @param size Maximum amount of bytes inflight

    Note. if using rabbitmq, the prefetch limits are set per consumer on the channel,
    rather than per channel (across consumers)
*)

val set_global_prefetch : ?count:int -> ?size:int -> _ t -> unit Deferred.t
(** Set global prefetch counters.
    @param count Maximum messages inflight (un-acked)
    @param size Maximum amount of bytes inflight

    Note: if using rabbitmq, the prefetch limits are set per channel (across consumers),
    If not, the global prefetch settings is applied globally - across consumers and channels.
*)


(** Flush the channel, making sure all messages have been sent *)
val flush : _ t -> unit Deferred.t

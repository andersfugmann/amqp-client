(** Operations on channels *)
open Thread
open Spec

(**/**)
type on_cancel = unit -> unit
type consumer = Basic.Deliver.t * Basic.Content.t * string -> unit
type consumers = (string, consumer * on_cancel) Hashtbl.t
(**/**)

type _ t

(**/**)
val channel : _ t -> Framing.t * int

module Internal : sig
  val register_consumer_handler : _ t -> string -> consumer -> on_cancel -> unit
  val deregister_consumer_handler : _ t -> string -> unit
  val wait_for_confirm : 'a t -> routing_key:string -> exchange_name:string -> 'a Deferred.t
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
  Framing.t -> Framing.channel_no -> 'a t Deferred.t

(** Close the channel *)
val close : _ t -> unit Deferred.t

(** [on_closed] becomes determined after then channel is closed.

    If there are no consumers of this when the channel is close
    [Connection_closed] will be raised to the governing exception
    handler (the parent monitor in async, or [Lwt.async_exception_hook]
    in lwt).
*)
val on_closed : _ t -> unit Deferred.t

(** Receive all returned messages. Reutnred message will be send to
    all readers returned from call to this function.  Listening for
    returned messages are useful in e.g. rpc to know that message
    delivery failed and then stop waiting for a response.

    Note that channels in ack mode there is no need to listen for
    returned messages, as message delivery will fail synchoniously.
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

(** Transactions.
    Transactions can be made per channel.

    After a transaction is started, all published messages and all changes
    (queue/exchange bindings, creations or deletions) and message
    acknowledgements are not visible outside the transaction.

    The changes becomes visible after a [commit] or canceled by call to [rollback].
*)
module Transaction : sig
  type tx

  (** Start a transacction *)
  val start : _ t -> tx Deferred.t

  (** Commit an transaction *)
  val commit : tx -> unit Deferred.t

  (** Rollback a transaction, discarding all changes and messages *)
  val rollback : tx -> unit Deferred.t
end

open Thread
open Amqp_client_lib

(** Operations on Queues *)
type t
type 'a consumer

val message_ttl : int -> string * Types.value
val auto_expire : int -> string * Types.value
val max_length : int -> string * Types.value
val max_length_bytes : int -> string * Types.value
val dead_letter_exchange : string -> string * Types.value
val dead_letter_routing_key : string -> string * Types.value
val maximum_priority : int -> string * Types.value

(** Declare a queue.

 To use server-generated queue name explicitly pass
 [~autogenerate:true] and empty name: [declare channel ~autogenerate:true ""].
 Reason for making [autogenerate] param explicit is inability in production
 to find out which services are leaking queues with auto-generated names.
 We advice not to use this feature in production.
*)
val declare :
  _ Channel.t ->
  ?durable:bool ->
  ?exclusive:bool ->
  ?auto_delete:bool ->
  ?passive:bool ->
  ?arguments:Types.table ->
  ?autogenerate:bool ->
  string -> t Deferred.t

(** Get a single message from the queue.
    The function automatically handles ack.

    If [no_ack] is false (default), the message is requsted with expicit
    ack and the caller is responsible for ack'ing or rejecting the message.
*)
val get :
  no_ack:bool ->
  _ Channel.t ->
  t -> Message.t option Deferred.t

(** Publish a message directly to a queue *)
val publish :
  'a Channel.t -> t ->
  ?mandatory:bool ->
  Message.message -> 'a Deferred.t

(** Setup consumption of a queue.  Remember to ack messages.

    All messages are processed concurrently.  To limit number of
    concurrent processes, set the prefetch threshold.

    [on_cancel] is called if the server cancels consumption. This may
    happen if e.g. the queue is deleted. If the argument is not
    provided and exception is raised.

*)
val consume :
  id:string ->
  ?no_local:bool ->
  ?no_ack:bool ->
  ?exclusive:bool ->
  ?on_cancel:(unit -> unit) ->
  'a Channel.t ->
  t ->
  ('a consumer * Message.t Pipe.Reader.t) Deferred.t


(** Cancel consumption. *)
val cancel : _ consumer -> unit Deferred.t

(** Bind a queue to an exchange.
    Messages posted on the exchange which match the routing key
    (and optionally match the headers)
    will be routed to the queue
*)
val bind : _ Channel.t -> t -> 'b Exchange.t -> 'b -> unit Deferred.t

(** Remove a binding from an exchange to a queue *)
val unbind : _ Channel.t -> t -> 'b Exchange.t -> 'b -> unit Deferred.t

(** Purge all messages on a queue *)
val purge : _ Channel.t -> t -> unit Deferred.t

(** Delete a queue *)
val delete :
  ?if_unused:bool ->
  ?if_empty:bool -> _ Channel.t -> t -> unit Deferred.t

(** Name of the queue *)
val name : t -> string

(**/**)
val fake : 'a -> string -> t Deferred.t
(**/**)

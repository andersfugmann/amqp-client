(** Operations on Queues *)
module Make : functor (Amqp_thread : Amqp_thread.T) -> sig
open Amqp_thread

type t
type 'a consumer

val message_ttl : int -> string * Amqp_types.value
val auto_expire : int -> string * Amqp_types.value
val max_length : int -> string * Amqp_types.value
val max_length_bytes : int -> string * Amqp_types.value
val dead_letter_exchange : string -> string * Amqp_types.value
val dead_letter_routing_key : string -> string * Amqp_types.value
val maximum_priority : int -> string * Amqp_types.value

(** Declare a queue *)
val declare :
  _ Amqp_channel.Make(Amqp_thread).t ->
  ?durable:Amqp_types.bit ->
  ?exclusive:Amqp_types.bit ->
  ?auto_delete:Amqp_types.bit ->
  ?arguments:Amqp_types.table ->
  string -> t Deferred.t

(** Get a single message from the queue.
    The function automatically handles ack.

    If [no_ack] is false (default), the message is requsted with expicit
    ack and the caller is responsible for ack'ing or rejecting the message.
*)
val get :
  no_ack:bool ->
  _ Amqp_channel.Make(Amqp_thread).t ->
  t -> Amqp_message.Make(Amqp_thread).t option Deferred.t

(** Publish a message directly to a queue *)
val publish :
  'a Amqp_channel.Make(Amqp_thread).t -> t ->
  ?mandatory:bool ->
  Amqp_message.Make(Amqp_thread).message -> 'a Deferred.t

(** Setup consumption of a queue.
    Remember to ack messages.

    All messages are processed concurrently.
    To limit number of concurrent processes, set the prefetch threashold.
*)
val consume :
  id:string ->
  ?no_local:bool ->
  ?no_ack:bool ->
  ?exclusive:bool ->
  'a Amqp_channel.Make(Amqp_thread).t ->
  t ->
  ('a consumer * Amqp_message.Make(Amqp_thread).t Pipe.Reader.t) Deferred.t


(** Cancel consumption. *)
val cancel : _ consumer -> unit Deferred.t

(** Bind a queue to an exhange.
    Messages posted on the exchange which match the routing key
    (and optionally match the headers)
    will be routed to the queue
*)
val bind : _ Amqp_channel.Make(Amqp_thread).t -> t -> 'b Amqp_exchange.Make(Amqp_thread).t -> 'b -> unit Deferred.t

(** Remove a binding from an exhange to a queue *)
val unbind : _ Amqp_channel.Make(Amqp_thread).t -> t -> 'b Amqp_exchange.Make(Amqp_thread).t -> 'b -> unit Deferred.t

(** Purge all messages on a queue *)
val purge : _ Amqp_channel.Make(Amqp_thread).t -> t -> unit Deferred.t

(** Delete a queue *)
val delete :
  ?if_unused:bool ->
  ?if_empty:bool -> _ Amqp_channel.Make(Amqp_thread).t -> t -> unit Deferred.t

(** Name of the queue *)
val name : t -> string

(**/**)
val fake : 'a -> string -> t Deferred.t
(**/**)
end

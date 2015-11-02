(** Operations on exchanges *)
open Async.Std

type _ t

type _ exchange_type =
  | Direct: unit exchange_type
  | Fanout: unit exchange_type
  | Topic: string exchange_type
  | Header: Amqp_types.header list exchange_type

val default : unit t
val amq_direct : unit t
val amq_fanout : unit t
val amq_topic : string t
val amq_match : Amqp_types.header list t

(**/**)
module Internal : sig
  val bind_queue : _ Amqp_channel.t -> 'a t -> string -> 'a -> unit Deferred.t
  val unbind_queue : _ Amqp_channel.t -> 'a t -> string -> 'a -> unit Deferred.t
end
(**/**)

(** Declare a exchange *)
val declare :
  ?passive:bool ->
  ?durable:bool ->
  ?auto_delete:bool ->
  _ Amqp_channel.t ->
  'a exchange_type ->
  string -> 'a t Deferred.t

(** Delete exhange *)
val delete :
  ?if_unused:bool ->
  _ Amqp_channel.t -> _ t -> unit Deferred.t

(** Bind exchange t to exchange using [routing_key] so messages are routed from exhange to [t] *)
val bind : _ Amqp_channel.t -> destination:_ t -> source:'a t -> 'a -> unit Deferred.t

(** Remove exchange to exchange binding *)
val unbind : _ Amqp_channel.t -> destination:_ t -> source:'a t -> 'a -> unit Deferred.t

(** Publish a message directly to an exchange.  *)
val publish :
  'a Amqp_channel.t ->
  _ t ->
  ?mandatory:bool ->
  routing_key:string ->
  Amqp_message.message -> 'a Deferred.t

(** Name of the exchange *)
val name : _ t -> string

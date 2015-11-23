(** Operations on exchanges *)
open Async.Std

type _ t

type _ exchange_type

val direct_t : (queue:string -> unit Deferred.t) exchange_type
val fanout_t : (unit Deferred.t) exchange_type
val topic_t  : (topic:string -> unit Deferred.t) exchange_type
val match_t  : (headers:(Amqp_types.header list) -> unit Deferred.t) exchange_type

val default : (queue:string -> unit Deferred.t) t
val amq_direct : (queue:string -> unit Deferred.t) t
val amq_fanout : (unit Deferred.t) t
val amq_topic : (topic:string -> unit Deferred.t) t
val amq_match : (headers:(Amqp_types.header list) -> unit Deferred.t) t

(**/**)
module Internal : sig
  val bind_queue : _ Amqp_channel.t -> 'a t -> string -> 'a
  val unbind_queue : _ Amqp_channel.t -> 'a t -> string -> 'a
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
val bind : _ Amqp_channel.t -> destination:_ t -> source:'a t -> 'a

(** Remove exchange to exchange binding *)
val unbind : _ Amqp_channel.t -> destination:_ t -> source:'a t -> 'a

(** Publish a message directly to an exchange. *)
val publish :
  'a Amqp_channel.t ->
  _ t ->
  ?mandatory:bool ->
  routing_key:string ->
  Amqp_message.message -> 'a Deferred.t

(** Name of the exchange *)
val name : _ t -> string

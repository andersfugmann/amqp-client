(** Operations on exchanges *)
open Thread
open Amqp_client_lib

type _ t

type _ exchange_type

val direct_t : [`Queue of string] exchange_type
val fanout_t : unit exchange_type
val topic_t  : [`Topic of string] exchange_type
val match_t  : [`Headers of Types.header list] exchange_type

val default    : [`Queue of string] t
val amq_direct : [`Queue of string] t
val amq_fanout : unit t
val amq_topic  : [`Topic of string] t
val amq_match  : [`Headers of Types.header list] t

(**/**)
module Internal : sig
  val bind_queue : _ Channel.t -> 'a t -> string -> 'a -> unit Deferred.t
  val unbind_queue : _ Channel.t -> 'a t -> string -> 'a -> unit Deferred.t
end
(**/**)

(** Declare a exchange *)
val declare :
  ?passive:bool ->
  ?durable:bool ->
  ?auto_delete:bool ->
  ?internal:bool ->
  _ Channel.t ->
  'a exchange_type ->
  ?arguments:Types.table ->
  string -> 'a t Deferred.t

(** Delete exchange *)
val delete :
  ?if_unused:bool ->
  _ Channel.t -> _ t -> unit Deferred.t

(** Bind exchange [t] to exchange using [routing_key], so messages are routed from exchange to [t] *)
val bind : _ Channel.t -> destination:_ t -> source:'a t -> 'a -> unit Deferred.t

(** Remove exchange to exchange binding *)
val unbind : _ Channel.t -> destination:_ t -> source:'a t -> 'a -> unit Deferred.t

(** Publish a message directly to an exchange. *)
val publish :
  'a Channel.t ->
  _ t ->
  ?mandatory:bool ->
  routing_key:string ->
  Message.message -> 'a Deferred.t

(** Name of the exchange *)
val name : _ t -> string

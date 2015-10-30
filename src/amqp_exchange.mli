(** Operations on exchanges *)
open Async.Std
type t

(** Predefined exchanges *)
val default : t
val amq_direct : t
val amq_fanout : t
val amq_topic : t
val amq_header : t

type exchange_type = Direct | Fanout | Topic | Headers

(** Declare a exchange *)
val declare :
  ?passive:bool ->
  ?durable:bool ->
  ?auto_delete:bool ->
  ?internal:bool ->
  _ Amqp_channel.t ->
  exchange_type ->
  string -> t Deferred.t

(** Delete exhange *)
val delete :
  ?if_unused:bool ->
  _ Amqp_channel.t -> t -> unit Deferred.t

(** Bind exchange t to exchange using [routing_key] so messages are routed from exhange to [t] *)
val bind :
  _ Amqp_channel.t ->
  t -> routing_key:Amqp_types.shortstr -> t -> unit Deferred.t

(** Remove exchange to exchange binding *)
val unbind :
  _ Amqp_channel.t ->
  t -> routing_key:string -> t -> unit Deferred.t

(** Publish a message directly to an exchange *)
val publish :
  'a Amqp_channel.t ->
  t ->
  ?mandatory:bool ->
  routing_key:string ->
  Amqp_message.message -> 'a Deferred.t

(** Name of the exchange *)
val name : t -> string

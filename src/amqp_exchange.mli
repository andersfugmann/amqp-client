open Async.Std
type t

(** Predefined exchanges *)
val default : t
val amq_direct : t
val amq_fanout : t
val amq_topic : t

type exchange_type = Direct | Fanout | Topic | Headers

val declare :
  ?passive:bool ->
  ?durable:bool ->
  ?auto_delete:bool ->
  ?internal:bool ->
  Amqp_channel.t ->
  exchange_type:exchange_type ->
  string -> t Async.Std.Deferred.t

val delete :
  ?if_unused:bool ->
  Amqp_channel.t -> t -> unit Deferred.t

val bind :
  Amqp_channel.t ->
  t -> routing_key:Amqp_types.shortstr -> t -> unit Deferred.t

val unbind :
  Amqp_channel.t ->
  t -> routing_key:string -> t -> unit Deferred.t

val publish :
  Amqp_channel.t ->
  t ->
  ?content_type:string ->
  ?content_encoding:string ->
  ?correlation_id:string ->
  ?message_id:string ->
  ?mandatory:bool ->
  ?reply_to:string ->
  ?expiration:string ->
  ?persistent:bool ->
  ?app_id:string ->
  ?headers:Amqp_types.table ->
  routing_key:string -> string -> unit Deferred.t

val name : t -> string

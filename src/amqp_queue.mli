(** Operations on Queues *)
open Async.Std

type t
type consumer

val message_ttl : int -> string * Amqp_types.value
val auto_expire : int -> string * Amqp_types.value
val max_length : int -> string * Amqp_types.value
val max_length_bytes : int -> string * Amqp_types.value
val dead_letter_exchange : string -> string * Amqp_types.value
val dead_letter_routing_key : string -> string * Amqp_types.value
val maximum_priority : int -> string * Amqp_types.value

(** Declare a queue *)
val declare :
  Amqp_channel.t ->
  ?durable:Amqp_types.bit ->
  ?exclusive:Amqp_types.bit ->
  ?auto_delete:Amqp_types.bit ->
  ?arguments:Amqp_types.table ->
  Amqp_spec.queue_name -> t Async.Std.Deferred.t

(** Get a single message from the queue.
    The function automatically handles ack.

    If [no_ack] is false (default), the message is requsted with expicit
    ack, and the function will ack the message when the hander returns
*)
val get :
  no_ack:Amqp_spec.no_ack ->
  Amqp_channel.t ->
  t ->
  (Amqp_spec.Basic.Get_ok.t ->
   Amqp_spec.Basic.Content.t -> string -> unit Deferred.t) ->
  unit Deferred.t

(** Publish a message directly to a queue *)
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
  ?headers:Amqp_types.table -> string -> unit Deferred.t

(** Setup consumption of a queue.
    The function handles ack when [no_ack] is false.
*)
val consume :
  id:string ->
  ?no_local:bool ->
  ?no_ack:bool ->
  ?exclusive:bool ->
  Amqp_channel.t ->
  t ->
  (Amqp_spec.Basic.Deliver.t ->
   Amqp_spec.Basic.Content.t -> string -> unit Async.Std.Deferred.t) ->
  consumer Deferred.t

(** Cancel consumption. *)
val cancel : consumer -> unit Deferred.t

(** Bind a queue to an exhange *)
val bind :
  Amqp_channel.t ->
  t ->
  routing_key:Amqp_types.shortstr ->
  Amqp_exchange.t -> unit Deferred.t

(** Remove a binding from an exhange to a queue *)
val unbind :
  Amqp_channel.t ->
  t ->
  routing_key:Amqp_types.shortstr ->
  Amqp_exchange.t -> unit Deferred.t

(** Purge all messages on a queue *)
val purge : Amqp_channel.t -> t -> unit Deferred.t

(** Delete a queue *)
val delete :
  ?if_unused:bool ->
  ?if_empty:bool -> Amqp_channel.t -> t -> unit Deferred.t

(** Name of the queue *)
val name : t -> string

(** Debuggin internal function *)
val fake : 'a -> string -> t Deferred.t

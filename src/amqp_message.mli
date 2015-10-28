(** Amqp message types *)
open Async.Std

type message = Amqp_spec.Basic.Content.t * string

type t = {
  delivery_tag : int;
  redelivered : bool;
  exchange : string;
  routing_key : string;
  message : message;
}

val make :
  ?content_type:string ->
  ?content_encoding:string ->
  ?headers:Amqp_types.table ->
  ?delivery_mode:int ->
  ?priority:int ->
  ?correlation_id:string ->
  ?reply_to:string ->
  ?expiration:int ->
  ?message_id:string ->
  ?timestamp:int ->
  ?amqp_type:string ->
  ?user_id:string -> ?app_id:string -> string -> message

(** Acknowledge a message.
    Messages {e must} be acknowledged on the same channel as they are received
*)
val ack: Amqp_channel.t -> t -> unit Deferred.t

(** Reject (Nack) a message.
    Messages {e must} be rejected on the same channel as they are received
    @param requeue If true, the message will be requeued (default)
*)
val reject: ?requeue:bool -> Amqp_channel.t -> t -> unit Deferred.t

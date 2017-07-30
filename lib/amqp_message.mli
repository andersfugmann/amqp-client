(** Amqp message type and functions *)

open Amqp_thread

type message = Amqp_spec.Basic.Content.t * string

val string_header: string -> string -> Amqp_types.header
val int_header: string -> int -> Amqp_types.header


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
val ack: _ Amqp_channel.t -> t -> unit Deferred.t

(** Reject (Nack) a message.
    Messages {e must} be rejected on the same channel as they are received
    @param requeue If true, the message will be requeued (default)
*)
val reject: requeue:bool -> _ Amqp_channel.t -> t -> unit Deferred.t

(** Ask the server to resend or discard all outstanding messages on the channel
    This is essentially the same as calling nack on all outstanding messages.
    @param requeue if true messages are redelivered
*)
val recover: requeue:bool -> _ Amqp_channel.t -> unit Deferred.t

(** Amqp message type and functions *)

open Thread
open Amqp_client_lib
type message = Spec.Basic.Content.t * string

val string_header: string -> string -> Types.header
val int_header: string -> int -> Types.header


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
  ?headers:Types.table ->
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
val ack: _ Channel.t -> t -> unit Deferred.t

(** Reject a message.
    Messages {e must} be rejected on the same channel as they are received
    @param requeue If true, the message will be requeued
*)
val reject: requeue:bool -> _ Channel.t -> t -> unit Deferred.t

(** Ask the server to resend or discard all outstanding messages on the channel
    This is essentially the same as calling nack on all outstanding messages.
    @param requeue if true messages are redelivered
*)
val recover: requeue:bool -> _ Channel.t -> unit Deferred.t


(** Count number of times a message has been redelivered.
    Amqp spec does not count number of message redeliveries.
    This function implement manual redeliver counting, by
    re-publishing the message whenever the redeliver flag is set,
    while adding a custom header to track how many times a message has been
    re-published (i.e. redelivered).

    [f] will only be called with messages which does not have the redelivered flag set.
    The default header name is 'x-redelivered-count'

    Message must be received in acknowledge mode
    (i.e. messages must be explicitly acknowledged / rejected),
    as this message may acknowledge some messages.

    The function does not use transaction, which means that a small window exists
    where a message will be duplicated.

*)
val with_redeliver_count:
  ?header_name:string -> [< `Failed | `Ok ] Channel.t ->
  f:(int -> t -> unit Deferred.t) -> t -> unit Deferred.t

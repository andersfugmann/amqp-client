(** Rpc client and server patterns *)
open Async.Std

(** Rpc Client pattern *)
module Client :
  sig
    type t

    (** Initialize a client with the [id] for tracing *)
    val init : id:string -> Amqp_connection.t -> t Deferred.t

    (** Make an rpc call to the given queue.
        [ttl] is the message timeout. *)
    val call :
      t ->
      ttl:int ->
      Amqp_queue.t -> string -> [ `Ok of string | `Timedout ] Deferred.t

    (** Release resources *)
    val close : t -> unit Deferred.t

  end

(** Rpc Server pattern *)
module Server :
  sig
    type t

    (** Start an rpc server procucing replies for requests comming in
        on the given queue. *)
    val start :
      Amqp_channel.t ->
      Amqp_queue.t ->
      (string -> string Deferred.t) -> t Async.Std.Deferred.t

    (** Stop the server *)
    val stop : t -> unit Deferred.t
  end

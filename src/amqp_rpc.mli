module Client :
  sig
    type t

    (** Initialize a client with the [id] for tracing *)
    val init : id:string -> Amqp_connection.t -> t Async.Std.Deferred.t

    (** Make an rpc call to the given queue.
        [ttl] is the message timeout.
    *)
    val call :
      t ->
      ttl:int ->
      Amqp_queue.t -> string -> [ `Ok of string | `Timedout ] Async.Std.Deferred.t
    (** Release resources *)
    val close : t -> unit Async.Std.Deferred.t
  end
module Server :
  sig
    type t
    (** Start an rpc server procucing replies for requests comming in
        on the given queue. *)
    val start :
      Amqp_channel.t ->
      Amqp_queue.t ->
      (string -> string Async.Std.Deferred.t) -> t Async.Std.Deferred.t

    (** Stop the server *)
    val stop : t -> unit Async.Std.Deferred.t
  end

module Client :
  sig
    type t
    val init : id:string -> Amqp_connection.t -> t Async.Std.Deferred.t

    val call :
      t ->
      ttl:int ->
      Amqp_queue.t -> string -> [ `Ok of string | `Timedout ] Async.Std.Deferred.t

    val close : t -> unit Async.Std.Deferred.t
  end
module Server :
  sig
    type t
    val init :
      Amqp_channel.t ->
      Amqp_queue.t ->
      (string -> string Async.Std.Deferred.t) -> t Async.Std.Deferred.t

    val stop : t -> unit Async.Std.Deferred.t
  end

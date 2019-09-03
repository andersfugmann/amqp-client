(** Rpc client and server patterns *)

open Thread
open Amqp_client_lib

(** Rpc Client pattern *)
module Client :
  sig
    type t

    (** Initialize a client with the [id] for tracing *)
    val init : id:string -> Connection.t -> t Deferred.t

    (** Make an rpc call to the exchange using the routing key and headers.
        @param ttl is the message timeout in milliseconds.
                   If the message is on the rpc endpoints queue for more than [ttl]
                   milliseconds the message will be dead
                   lettered and returned which will cause this function to timeout and
                   return None.

        @param grace_time_ms is the time added to the ttl before the function times out and returns None
                             This is to give the rpc serve a chance to process the message,
                             in case the rpc server consumed the message from the queue close to ttl.
                             Default 100ms.

        To call directly to a named queue, use
        [call t Exchange.default ~ttl:500 ~routing_key:"name_of_the_queue" ~headers:[]]

        [correlation_id] allows you to specify a correlation id. The
        id will be suffixed with an id to allow the caller to reuse
        correlation ids. This can be used for tracing by reusing
        correlation ids of incomming requests resulting in new
        calls. If no correlation is given the id of the [client] is used.

        The function allows the call to specify both a routing key and
        headers regardless of the type of exchange used, as exchanges
        may be chained in a way where both headers and routing keys
        are used.

        This function will timeout and return None, either if the request message is
        dead-lettered or if ttl + grace_time_ms has passed.
    *)
    val call :
      t ->
      ?correlation_id:string ->
      ttl:int ->
      ?grace_time_ms:int ->
      routing_key:string ->
      headers:Types.header list ->
      _ Exchange.t ->
      Spec.Basic.Content.t * string ->
      Message.message option Deferred.t

    (** Release resources *)
    val close : t -> unit Deferred.t
  end

(** Rpc Server pattern *)
module Server :
  sig
    type 'a t

    (** Recommended argument to add when declaring the rpc server queue.
        This will set the dead letter exchange to the header exchange to help
        clients to be notified if a request has timed out
    *)
    val queue_argument : Types.header

    (** Start an rpc server producing replies for requests coming in
        on the given queue.
        @param async If true, multiple requests can be handled concurrently.
                     If false, message are handled synchronously (default)

        It is recommended to create the queue with the header_exchange
        as dead letter exchange.  This will allow messages to be routed
        back the the sender at timeout. E.g:
        [ Queue.declare ~arguments:[Rpc.queue_argument] "rpcservice" ]
    *)
    val start :
      ?async:bool -> ?discard_redelivered:bool ->
      ([< `Failed | `Ok ] as 'a) Channel.t ->
      Queue.t ->
      (Message.message -> Message.message Deferred.t) -> 'a t Deferred.t

    (** Stop the server *)
    val stop : _ t -> unit Deferred.t
  end

(** Rpc client and server patterns *)

open Thread

(** Rpc Client pattern *)
module Client :
  sig
    type t

    (** Initialize a client with the [id] for tracing *)
    val init : id:string -> Connection.t -> t Deferred.t

    (** Make an rpc call to the exchange using the routing key and headers.
        @param ttl is the message timeout.

        To call directly to a named queue, use
        [call t Exchange.default ~routing_key:"name_of_the_queue" ~headers:[]]

        [correlation_id] allows you to specify a correlation id. The
        id will be suffixed with an id to allow the caller to reuse
        correlation ids. This can be used for tracing by reusing
        correlation ids of incomming requests resulting in new
        calls. If no correlation is given the id of the [client] is used.

        The function allows the call to specify both a routing key and
        headers regardless of the type of exchange used, as exchanges
        may be chained in a way where both headers and routing keys
        are used.
    *)
    val call :
      t ->
      ?correlation_id:string ->
      ttl:int ->
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

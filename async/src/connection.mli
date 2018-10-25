open Thread

(** Connection *)
type t

(** Connect to an Amqp server.

    [connect ~id:"test" localhost] connects to localhost using default guest credentials, with identity "test"

    @param id an identifier of the connection used for tracing and debugging
    @param credentials a tuple of username * password. The credentials are transmitted in plain text
    @param virtual_host Named of the virtual host.
           Virtual must be defined on the amqp-server prior to connecting them.
           Default "/"
    @param port The port to connect to
    @param heartbeat Delay between heartbeats in seconds. Lower the number to detect connection loss faster.


    If an error occurs an exception is raised. To capture and handle
    exceptions it is advised to detach a monitor [Core.Async.Monitor]
    and handle raised exceptions.

    The most important exception is [Connection_closed]. As the
    connection is stateful (channels are tied to connections e.g),
    the connection cannot be re-established without redoing all
    initalization.
*)
val connect :
  id:string ->
  ?virtual_host:string ->
  ?port:int ->
  ?credentials:string * string ->
  ?heartbeat:int ->
  string -> t Deferred.t

(** Connect to amqp using an uri.

    [connect_uri ~id:"test" "amqp://localhost/"] connects to amqp server on localhost using default port and default username/password.

    The uri must be on the form: [ampq://user:password@hostname:port/vhost?params].
    Currently only 'heartbeat_interval=<sec>' parameter is used.
*)
val connect_uri :
  id:string ->
  string -> t Deferred.t

(** Open a new channel.
    @param id identifies the channel for tracing and debugging
*)
val open_channel : id:string -> 'a Channel.confirms -> t -> 'a Channel.t Deferred.t
val close : t -> unit Deferred.t

(** [on_closed] becomes ready when the connection has been closed. *)
val on_closed : t -> unit Deferred.t

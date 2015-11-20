(** Connection *)
open Async.Std
type t

(** Connect to an Amqp server.

    [connect ~id:"test" localhost] connects to localhost using default guest credentials, with identity "test"

    @param id an identifier of the connection used for tracing and debugging
    @param credentials a tuple of username * password. The credentials are transmitted in plain text
    @param virtual_host Named of the virtual host.
           Virtual must be defined on the amqp-server prior to connecting them.
           Default "/"
    @param port The port to connect to
    @param heartbeat delay between hearbeats in seconds. Lower the number to detect copnnection loss faster.


    If an error occurs an excaption is raised. To capture and handle
    exceptions it is adviced to detatch a monitor [Core.Async.Monitor]
    and handle raised exceptions.

    The most important exception is [Connection_closed]. As the
    connection is statefull (channels are tied to connections e.g),
    the connection cannot be restablished without redoing all
    initalization.
*)
val connect :
  id:string ->
  ?virtual_host:string ->
  ?port:int ->
  ?credentials:string * string ->
  ?heartbeat:int ->
  string -> t Deferred.t

(** Open a new channel.
    @param id identifies the channel for tracing and debugging
*)
val open_channel : id:string -> 'a Amqp_channel.confirms -> t -> 'a Amqp_channel.t Deferred.t
val close : t -> unit Deferred.t

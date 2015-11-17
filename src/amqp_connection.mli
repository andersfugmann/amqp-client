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
    @param exn_handler If given, all caught exceptions (Such as Connection_closed) will be sent to the handler though a detached monitor. If no exception hander is given no monitor will be installed, and exception will be passed up the hierachy. See [Core.Async.Monitor]


*)
val connect :
  id:string ->
  ?exn_handler:(exn -> unit) ->
  ?virtual_host:string ->
  ?port:int ->
  ?credentials:string * string -> string -> t Deferred.t

(** Open a new channel.
    @param id identifies the channel for tracing and debugging
*)
val open_channel : id:string -> 'a Amqp_channel.confirms -> t -> 'a Amqp_channel.t Deferred.t
val close : t -> unit Deferred.t

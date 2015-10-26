(** Connection *)
open Async.Std
type t
type host = string

(** Connect to an Amqp server.
    @param id is an identifier of the connection used for tracing and debugging
    @param credentials a tuple of username * password. The credentials are transmitted as plain text
*)
val connect :
  id:string ->
  ?virtual_host:string ->
  ?port:int ->
  ?credentials:string * string -> host -> t Deferred.t

(** Open a new channel.
    [id] identifies the channel for tracing and debugging
*)
val open_channel : id:string -> t -> Amqp_channel.t Deferred.t

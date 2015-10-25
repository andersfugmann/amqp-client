open Async.Std
type t

val open_connection : t -> unit Deferred.t

val connect :
  id:string ->
  ?virtual_host:string ->
  ?port:int ->
  ?credentials:string * string -> string -> t Deferred.t

val open_channel : id:string -> t -> Amqp_channel.t Deferred.t

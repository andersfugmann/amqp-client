open Amqp_spec

type message = (Basic.Content.t * string)

type t =
  { delivery_tag : int;
    redelivered : bool;
    exchange : string;
    routing_key : string;
    message: message;
  }

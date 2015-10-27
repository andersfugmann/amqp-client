open Amqp_spec

type message = (Basic.Content.t * string)

type deliver =
  { consumer_tag : string;
    delivery_tag : int;
    redelivered : bool;
    exchange : string;
    routing_key : string;
    message: message;
  }

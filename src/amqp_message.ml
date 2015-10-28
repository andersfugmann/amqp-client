open Amqp_spec.Basic

type message = (Content.t * string)

type t =
  { delivery_tag : int;
    redelivered : bool;
    exchange : string;
    routing_key : string;
    message: message;
  }

let make
    ?(content_type:string option)
    ?(content_encoding: string option)
    ?(headers: Amqp_types.table option)
    ?(delivery_mode: int option)
    ?(priority: int option)
    ?(correlation_id: string option)
    ?(reply_to: string option)
    ?(expiration: int option)
    ?(message_id: string option)
    ?(timestamp: int option)
    ?(amqp_type: string option)
    ?(user_id: string option)
    ?(app_id: string option)
    body : message =
  let expiration = match expiration with
    | None -> None
    | Some n -> Some (string_of_int n)
  in

  ({ Content.content_type;
     content_encoding;
     headers;
     delivery_mode;
     priority;
     correlation_id;
     reply_to;
     expiration;
     message_id;
     timestamp;
     amqp_type;
     user_id;
     app_id;
     reserved = None;
   }, body)

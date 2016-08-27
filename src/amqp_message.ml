open Amqp_spec.Basic
type message = (Content.t * string)

let string_header key value = key, Amqp_types.VLongstr value
let int_header key value = key, Amqp_types.VLonglong value

type t =
  { delivery_tag : int;
    redelivered : bool;
    exchange : string;
    routing_key : string;
    message: message; (* Could be in or out of the record *)
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

let ack channel t =
  let open Amqp_spec.Basic in
  Ack.request (Amqp_channel.channel channel)
    { Ack.delivery_tag = t.delivery_tag; multiple = false }

let reject ~requeue channel t =
  let open Amqp_spec.Basic in
  Reject.request (Amqp_channel.channel channel)
    { Reject.delivery_tag = t.delivery_tag; requeue }


let recover ~requeue channel =
  Amqp_spec.Basic.Recover.request  (Amqp_channel.channel channel) { Amqp_spec.Basic.Recover.requeue }

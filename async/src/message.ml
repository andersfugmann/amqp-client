open Spec.Basic
open Amqp_client_lib

type message = (Content.t * string)

let string_header key value = key, Types.VLongstr value
let int_header key value = key, Types.VLonglong value

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
    ?(headers: Types.table option)
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
  let open Spec.Basic in
  Ack.request (Channel.channel channel)
    { Ack.delivery_tag = t.delivery_tag; multiple = false }

let reject ~requeue channel t =
  let open Spec.Basic in
  Reject.request (Channel.channel channel)
    { Reject.delivery_tag = t.delivery_tag; requeue }


let recover ~requeue channel =
  Spec.Basic.Recover.request  (Channel.channel channel) { Spec.Basic.Recover.requeue }

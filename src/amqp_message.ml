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
    ?content_type
    ?content_encoding
    ?headers
    ?delivery_mode
    ?priority
    ?correlation_id
    ?reply_to
    ?expiration
    ?message_id
    ?timestamp
    ?amqp_type
    ?user_id
    ?app_id
    ?reserved body : message =
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
     reserved;
   }, body)

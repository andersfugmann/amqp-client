open Thread
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

let with_redeliver_count ?(header_name="x-redelivered-count") channel ~f t =
  let get_redelivered_count headers =
    match List.assoc header_name headers with
    | Types.VLong n
    | Types.VLonglong n
    | Types.VShort n
    | Types.VShortshort n -> n
    | _ -> 0
    | exception Not_found -> 0
  in
  let set_redelivered_count count headers =
    let filtered = List.remove_assoc header_name headers in
    (header_name, Types.VShort count) :: filtered
  in

  let headers = match (fst t.message).headers with
    | Some h -> h
    | None -> []
  in
  let redeliver_count = get_redelivered_count headers in

  match t.redelivered with
  | false -> f redeliver_count t >>= fun () ->
    return ()

  | true ->
    let headers = set_redelivered_count (redeliver_count + 1) headers in
    Channel.publish channel ~exchange_name:t.exchange ~routing_key:t.routing_key ({ (fst t.message) with headers = Some headers}, snd t.message) >>= function
    | `Ok ->
      ack channel t
    | `Failed ->
      failwith "Unable to repost message"

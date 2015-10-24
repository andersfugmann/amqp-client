open Amqp_spec.Exchange

type t = { name : string }

let default = { name="" }
let amq_direct = { name = "amq.direct" }
let amq_fanout = { name = "amq.fanout" }
let amq_topic = { name = "amq.topic" }

let init = () (* declare *)
let delete = ()

(* Bind exchanges *)
let bind = ()
let unbind = ()

let publish channel { name }
    ?content_type
    ?content_encoding
    ?correlation_id
    ?message_id
    ?(mandatory=false)
    ?reply_to
    ?expiration
    ?(persistent=false)
    ?app_id
    ?headers
    ~routing_key
    data =
  let open Amqp_spec.Basic in
  let app_id = match app_id with
    | Some "" -> None
    | Some n -> Some n
    | None -> Some (Amqp_channel.id channel)
  in
  let delivery_mode = if persistent then Some 2 else None in
  Publish.request (Amqp_channel.channel channel)
    ({Publish.exchange = name; routing_key=routing_key; mandatory; immediate=false},
     (Content.init ?content_type
        ?content_encoding
        ?correlation_id
        ?reply_to
        ?message_id
        ?delivery_mode
        ?app_id
        ?headers
        ?expiration ()), data)

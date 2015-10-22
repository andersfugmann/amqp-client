open Async.Std
open Amqp_protocol
open Amqp_spec

type message = Basic.Deliver.t * (Basic.Content.t * string)
type consumers = (string, message -> unit) Hashtbl.t
type t = { framing: Amqp_framing.t;
           channel_no: int;
           consumers: consumers;
           id: string;
           mutable counter: int;
         }

let handle_channel_open_ok () =
  log "Open_ok";
  return ()

let channel { framing; channel_no; _ } = (framing, channel_no)

let register_deliver_handler =
  let open Amqp_spec.Basic in
  let ((c_class_id, _), c_spec, c_make, _apply) = Content.I.def in
  let (message_id, spec, make, _apply) = Deliver.I.def in

  let c_read = Amqp_types.Content.read c_spec in
  let read = Amqp_types.Spec.read spec in

  let content_handler channel handler deliver (content, data) =
    let property_flag = Amqp_util.read_property_flag (Input.short content) in
    let header = c_read c_make property_flag content in
    Amqp_framing.deregister_content_handler channel c_class_id;
    handler (deliver, (header, data))
  in
  let deliver_handler channel consumers input =
    let deliver = read make input in
    try
      let handler = Hashtbl.find consumers deliver.Deliver.consumer_tag in
      Amqp_framing.register_content_handler channel c_class_id (content_handler channel handler deliver)
    (* Keep the current handler *)
    with
    | Not_found -> failwith ("No consumers for: " ^ deliver.Deliver.consumer_tag)
  in
  fun t ->
    let channel = channel t in
    Amqp_framing.register_method_handler channel message_id (deliver_handler channel t.consumers)

let register_consumer_handler t consumer_tag handler =
  if Hashtbl.mem t.consumers consumer_tag then raise Amqp_framing.Busy;
  Hashtbl.add t.consumers consumer_tag handler

let deregister_consumer_handler t consumer_tag =
  Hashtbl.remove t.consumers consumer_tag

let init ~id framing channel_no  =
  let consumers = Hashtbl.create 0 in
  let id = Printf.sprintf "%s.%s" (Amqp_framing.id framing) id in
  let t = { framing; channel_no; consumers; id; counter = 0 } in
  Amqp_framing.open_channel framing channel_no >>= fun () ->
  Amqp_spec.Channel.Open.request (channel t) () >>=
  handle_channel_open_ok >>= fun () ->
  register_deliver_handler t;
  return t

let close { framing; channel_no; _ } =
  Amqp_framing.close_channel framing channel_no

let next_counter t =
  t.counter <- t.counter + 1;
  t.counter

let id t = t.id

let unique_id t =
  Printf.sprintf "%s.%d" t.id (next_counter t);

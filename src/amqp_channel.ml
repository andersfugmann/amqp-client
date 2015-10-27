open Async.Std
open Amqp_io
open Amqp_spec

type message = Basic.Deliver.t * (Basic.Content.t * string)
type consumers = (string, message -> unit) Hashtbl.t
type close_handler = int -> Amqp_spec.Channel.Close.t -> unit Deferred.t
type t = { framing: Amqp_framing.t;
           channel_no: int;
           consumers: consumers;
           id: string;
           mutable counter: int;
           mutable close_handler: close_handler;
         }

let channel { framing; channel_no; _ } = (framing, channel_no)

module Internal = struct
  let next_counter t =
    t.counter <- t.counter + 1;
    t.counter

  let unique_id t =
    Printf.sprintf "%s.%d" t.id (next_counter t)

  let register_deliver_handler =
    let open Basic in
    let ((c_class_id, _), c_spec, c_make, _apply) = Content.Internal.def in
    let (message_id, spec, make, _apply) = Deliver.Internal.def in

    let c_read = Amqp_protocol.Content.read c_spec in
    let read = Amqp_protocol.Spec.read spec in
    let flags = Amqp_protocol.Content.length c_spec in

    let content_handler channel handler deliver (content, data) =
      let property_flag = Amqp_protocol_helpers.read_property_flag (Input.short content) flags in
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
end

let close_handler channel_no close =
  eprintf "Channel closed: %d\n" channel_no;
  eprintf "Reply code: %d\n" close.Channel.Close.reply_code;
  eprintf "Reply text: %s\n" close.Channel.Close.reply_text;
  eprintf "Message: (%d, %d)\n" close.Channel.Close.class_id close.Channel.Close.method_id;
  Shutdown.shutdown 1;
  return ()

let create ~id framing channel_no  =
  let consumers = Hashtbl.create 0 in
  let id = Printf.sprintf "%s.%s.%d" (Amqp_framing.id framing) id channel_no in
  let t = { framing; channel_no; consumers; id; counter = 0; close_handler } in
  Amqp_framing.open_channel framing channel_no >>= fun () ->
  Channel.Open.request (channel t) () >>= fun () ->
  Internal.register_deliver_handler t;
  don't_wait_for (Channel.Close.reply (framing, channel_no) (t.close_handler channel_no));

  return t

let register_close_handler t handler =
  t.close_handler <- handler

let close { framing; channel_no; _ } =
  let open Channel.Close in
  request (framing, channel_no)
    { reply_code=200;
      reply_text="Closed on user request";
      class_id=0;
      method_id=0; } >>= fun () ->
  Amqp_framing.close_channel framing channel_no;
  return ()

(* TODO: Rename this method *)
let on_return t handler =
  let open Basic.Return in
  let rec read () =
    don't_wait_for (reply (channel t) ~post_handler:(fun _ -> read ()) >>= fun msg -> handler msg) in
  read ()

let id t = t.id

let channel_no t = t.channel_no

let set_prefetch ?(count=0) ?(size=0) ?(global=false) t =
  Basic.Qos.request (channel t) { Basic.Qos.prefetch_count=count;
                      prefetch_size=size;
                      global }

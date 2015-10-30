open Async.Std
open Amqp_io
open Amqp_spec


type no_confirm = [ `Ok ]
type with_confirm = [ `Ok | `Failed ]

type _ confirms =
  | No_confirm: no_confirm confirms
  | With_confirm: with_confirm confirms

let no_confirm = No_confirm
let with_confirm = With_confirm

type consumer = Basic.Deliver.t * Basic.Content.t * string -> unit
type consumers = (string, consumer) Hashtbl.t

type publish_confirm = { mutable message_count: int;
                         unacked: (int * [ `Ok | `Failed ] Ivar.t) Core.Doubly_linked.t }

type _ pcp =
  | Pcp_no_confirm: [ `Ok ] pcp
  | Pcp_with_confirm: publish_confirm -> [ `Ok | `Failed ] pcp

type close_handler = int -> Channel.Close.t -> unit Deferred.t
type 'a t = { framing: Amqp_framing.t;
           channel_no: int;
           consumers: consumers;
           id: string;
           mutable counter: int;
           publish_confirm: 'a pcp;
         }

let channel { framing; channel_no; _ } = (framing, channel_no)

module Internal = struct
  type e = E: _ t -> e

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

    let content_handler channel handler deliver (header, body) =
      let module D = Deliver in
      let property_flag = Amqp_protocol_helpers.read_property_flag (Input.short header) flags in
      let header = c_read c_make property_flag header in
      Amqp_framing.deregister_content_handler channel c_class_id;
      handler (deliver, header, body)
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
    fun channel consumers ->
      Amqp_framing.register_method_handler channel message_id (deliver_handler channel consumers)

  let register_consumer_handler t consumer_tag handler =
    if Hashtbl.mem t.consumers consumer_tag then raise Amqp_framing.Busy;
    Hashtbl.add t.consumers consumer_tag handler

  let deregister_consumer_handler t consumer_tag =
    Hashtbl.remove t.consumers consumer_tag

  let wait_for_confirm: type a. a t -> a Deferred.t = fun t ->
    match t.publish_confirm with
    | Pcp_with_confirm t ->
      let var = Ivar.create () in
      let id = t.message_count + 1 in
      t.message_count <- id;
      let (_:'a Core.Doubly_linked.Elt.t) = Core.Doubly_linked.insert_last t.unacked (id, var) in
      (Ivar.read var : [`Ok | `Failed] Deferred.t)
    | Pcp_no_confirm -> return `Ok
end

let close_handler channel_no close =
  eprintf "Channel closed: %d\n" channel_no;
  eprintf "Reply code: %d\n" close.Channel.Close.reply_code;
  eprintf "Reply text: %s\n" close.Channel.Close.reply_text;
  eprintf "Message: (%d, %d)\n" close.Channel.Close.class_id close.Channel.Close.method_id;
  Shutdown.shutdown 1;
  return ()

let handle_confirms =
  let module Dl = Core.Doubly_linked in
  let open Basic in

  let (a_message_id, a_spec, a_make, _apply) = Ack.Internal.def in
  let a_read = Amqp_protocol.Spec.read a_spec in

  let (r_message_id, r_spec, r_make, _apply) = Reject.Internal.def in
  let r_read = Amqp_protocol.Spec.read r_spec in

  let handle t s tag = function
    | true ->
      let rec loop () =
        match Dl.first (t.unacked) with
        | Some (id, var) when id <= tag ->
          Ivar.fill var s;
          let (_: 'a option) = Dl.remove_first t.unacked in
          loop ()
        | Some _
        | None -> ()
      in
      loop ()
    | false ->
      begin match Dl.find_elt t.unacked ~f:(fun (id, _) -> id = tag) with
        | Some elt ->
          let (_, var) = Dl.Elt.value elt in
          Ivar.fill var s;
          Dl.remove t.unacked elt
        | None ->
          failwith (Printf.sprintf "Unexpected confirm: %d %d"
                      tag
                      (Dl.length t.unacked))
      end
  in
  let handle_ack t input =
    let ack = a_read a_make input in
    handle t `Ok ack.Ack.delivery_tag ack.Ack.multiple
  in

  let handle_reject t input =
    let reject = r_read r_make input in
    handle t `Failed reject.Reject.delivery_tag false
  in

  fun channel t ->
    (* Register for confirms *)
    Amqp_spec.Confirm.Select.request channel { Amqp_spec.Confirm.Select.nowait = false } >>= fun () ->
    Amqp_framing.register_method_handler channel a_message_id (handle_ack t);
    Amqp_framing.register_method_handler channel r_message_id (handle_reject t);
    return ()

let create: type a. id:string -> a confirms -> Amqp_framing.t -> Amqp_framing.channel_no -> a t Deferred.t = fun ~id confirm_type framing channel_no ->
  let consumers = Hashtbl.create 0 in
  let id = Printf.sprintf "%s.%s.%d" (Amqp_framing.id framing) id channel_no in
  Amqp_framing.open_channel framing channel_no >>= fun () ->
  Channel.Open.request (framing, channel_no) () >>= fun () ->
  Internal.register_deliver_handler (framing, channel_no) consumers;
  (* TODO: Use correct updated close handler *)
  don't_wait_for (Channel.Close.reply (framing, channel_no) (close_handler channel_no));
  let publish_confirm : a pcp = match confirm_type with
    | With_confirm ->
        Pcp_with_confirm { message_count = 0; unacked = Core.Doubly_linked.create () }
    | No_confirm -> Pcp_no_confirm
  in

  (match publish_confirm with Pcp_with_confirm t -> handle_confirms (framing, channel_no) t | Pcp_no_confirm -> return ()) >>= fun () ->
  let t = { framing; channel_no; consumers; id; counter = 0; publish_confirm } in
  return t

let register_close_handler _t _handler = ()

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

let set_prefetch ?(count=0) ?(size=0) t =
  Basic.Qos.request (channel t) { Basic.Qos.prefetch_count=count;
                                  prefetch_size=size;
                                  global=false }

let set_global_prefetch ?(count=0) ?(size=0) t =
  Basic.Qos.request (channel t) { Basic.Qos.prefetch_count=count;
                                  prefetch_size=size;
                                  global=true }

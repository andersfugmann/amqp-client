open Async.Std
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
  | Pcp_no_confirm: no_confirm pcp
  | Pcp_with_confirm: publish_confirm -> with_confirm pcp

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
  let next_counter t =
    t.counter <- t.counter + 1;
    t.counter

  let unique_id t =
    Printf.sprintf "%s.%d" t.id (next_counter t)

  let register_deliver_handler t =
    let open Basic in
    let handler (deliver, (content, data)) =
      try
        let handler = Hashtbl.find t.consumers deliver.Deliver.consumer_tag in
        handler (deliver, content, data);
      (* Keep the current handler *)
      with
      | Not_found -> failwith ("No consumers for: " ^ deliver.Deliver.consumer_tag)
    in
    let read = snd Deliver.Internal.read in
    read ~once:false handler (channel t)

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

let register_close_handler t handler =
  don't_wait_for (Channel.Close.reply (channel t) (handler t.channel_no))

let handle_confirms channel t =
  let module Dl = Core.Doubly_linked in
  let open Basic in

  (* Handle ack or nack *)
  let handle s tag = function
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
  let read_ack = snd Ack.Internal.read in
  let read_reject = snd Reject.Internal.read in
  read_ack ~once:false (fun m -> handle `Ok m.Ack.delivery_tag m.Ack.multiple) channel;
  read_reject ~once:false (fun m -> handle `Failed m.Reject.delivery_tag false) channel;
  Confirm.Select.request channel { Confirm.Select.nowait = false }

let create: type a. id:string -> a confirms -> Amqp_framing.t -> Amqp_framing.channel_no -> a t Deferred.t = fun ~id confirm_type framing channel_no ->
  let consumers = Hashtbl.create 0 in
  let id = Printf.sprintf "%s.%s.%d" (Amqp_framing.id framing) id channel_no in
  Amqp_framing.open_channel framing channel_no >>= fun () ->
  Channel.Open.request (framing, channel_no) () >>= fun () ->
  let publish_confirm : a pcp = match confirm_type with
    | With_confirm ->
        Pcp_with_confirm { message_count = 0; unacked = Core.Doubly_linked.create () }
    | No_confirm -> Pcp_no_confirm
  in

  (match publish_confirm with Pcp_with_confirm t -> handle_confirms (framing, channel_no) t | Pcp_no_confirm -> return ()) >>= fun () ->
  let t = { framing; channel_no; consumers; id; counter = 0; publish_confirm } in
  Internal.register_deliver_handler t;

  register_close_handler t close_handler;
  return t

let close { framing; channel_no; _ } =
  let open Channel.Close in
  request (framing, channel_no)
    { reply_code=200;
      reply_text="Closed on user request";
      class_id=0;
      method_id=0; } >>= fun () ->
  Amqp_framing.close_channel framing channel_no;
  return ()

let on_return t =
  let reader, writer = Pipe.create () in
  let (_, read) = Basic.Return.Internal.read in
  read ~once:false (Pipe.write_without_pushback writer) (channel t);
  reader

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

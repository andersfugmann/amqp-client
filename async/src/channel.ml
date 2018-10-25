open Thread
open Spec
open Amqp_client_lib

type no_confirm = [ `Ok ]
type with_confirm = [ `Ok | `Failed ]


type _ confirms =
  | No_confirm: no_confirm confirms
  | With_confirm: with_confirm confirms

let no_confirm = No_confirm
let with_confirm = With_confirm

type on_cancel = unit -> unit
type consumer = Basic.Deliver.t * Basic.Content.t * string -> unit
type consumers = (string, consumer * on_cancel) Hashtbl.t

type result = Delivered | Rejected | Undeliverable
type message_info = { delivery_tag: int;
                      routing_key: string;
                      exchange_name: string;
                      result_handler: result -> unit;
                    }

type publish_confirm = { mutable message_count: int;
                         unacked: message_info Mlist.t }

type _ pcp =
  | Pcp_no_confirm: no_confirm pcp
  | Pcp_with_confirm: publish_confirm -> with_confirm pcp

type return_handler = (Basic.Return.t * (Basic.Content.t * string)) option -> unit

type 'a t = { framing: Framing.t;
              channel_no: int;
              consumers: consumers;
              id: string;
              mutable counter: int;
              publish_confirm: 'a pcp;
              mutable return_handlers: return_handler list;
              mutable closed: unit Ivar.t option;
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
      match Hashtbl.find t.consumers deliver.Deliver.consumer_tag with
      | handler, _ ->
        handler (deliver, content, data);
        (* Keep the current handler *)
      | exception Not_found ->
        failwith ("No consumers for: " ^ deliver.Deliver.consumer_tag)
    in
    let read = snd Deliver.Internal.read in
    read ~once:false handler (channel t)

  let register_consumer_handler t consumer_tag handler on_cancel =
    if Hashtbl.mem t.consumers consumer_tag then raise Types.Busy;
    Hashtbl.add t.consumers consumer_tag (handler, on_cancel)

  let deregister_consumer_handler t consumer_tag =
    Hashtbl.remove t.consumers consumer_tag

  let set_result ivar = function
    | Delivered ->
      Ivar.fill ivar `Ok
    | Rejected ->
      Ivar.fill ivar `Failed
    | Undeliverable ->
      Ivar.fill ivar `Failed

  (* Need to add if we should expect returns also. *)
  let wait_for_confirm: type a. a t -> routing_key:string -> exchange_name:string -> a Deferred.t = fun t ~routing_key ~exchange_name ->
    match t.publish_confirm with
    | Pcp_with_confirm t ->
      let var = Ivar.create () in
      let result_handler = set_result var in
      t.message_count <- t.message_count + 1;
      let delivery_tag = t.message_count in
      Mlist.append t.unacked {delivery_tag; routing_key; exchange_name; result_handler};
      (Ivar.read var : [`Ok | `Failed] Deferred.t)
    | Pcp_no_confirm -> return `Ok
end

let close_handler t channel_no close =
  Log.info "Channel closed: %d" channel_no;
  Log.info "Reply code: %d\n" close.Channel.Close.reply_code;
  Log.info "Reply text: %s\n" close.Channel.Close.reply_text;
  Log.info "Message: (%d, %d)\n" close.Channel.Close.class_id close.Channel.Close.method_id;
  match t.closed with
  | None -> raise (Types.Channel_closed channel_no)
  | Some ivar ->
    Ivar.fill ivar ();
    return ()

let consumer_cancel_handler t (cancel : Basic.Cancel.t) =
  let consumer_tag = cancel.Basic.Cancel.consumer_tag in
  match Hashtbl.find t.consumers consumer_tag with
  | _, on_cancel ->
    Hashtbl.remove t.consumers consumer_tag;
    on_cancel ()
  | exception Not_found ->
    failwith
      ("Cannot cancel consumer, as no handler was found for consumer: " ^ consumer_tag)

let register_flow_handler t =
  let (_, read) = Channel.Flow.Internal.read in
  let handler { Channel.Flow.active } =
    Framing.set_flow t.framing t.channel_no active;
    spawn (Channel.Flow_ok.Internal.write (channel t) { Channel.Flow_ok.active })
  in
  read ~once:false handler (channel t)

let handle_confirms channel t =
  let confirm multiple result tag =
    let results = match multiple with
      | true -> Mlist.take_while ~pred:(fun m -> m.delivery_tag <= tag) t.unacked
      | false -> Mlist.take ~pred:(fun m -> m.delivery_tag = tag) t.unacked
                 |> Option.map_default ~f:(fun v -> [v]) ~default:[]
    in
    List.iter (fun m -> m.result_handler result) results
  in

  let return_handler = function
    | Some (r, _) -> begin
        let pred message =
          message.routing_key = r.Basic.Return.routing_key && message.exchange_name = r.Basic.Return.exchange
        in

        match Mlist.take ~pred t.unacked with
        | None -> Log.error "No messages found to mark as undeliverable. This would indicate a library error"
        | Some m -> m.result_handler Undeliverable
      end
    | None -> ()
  in

  let open Basic in
  let read_ack = snd Ack.Internal.read in
  let read_reject = snd Reject.Internal.read in
  read_ack ~once:false (fun m -> confirm m.Ack.multiple Delivered m.Ack.delivery_tag) channel;
  read_reject ~once:false (fun m -> confirm false Rejected m.Reject.delivery_tag) channel;

  Confirm.Select.request channel { Confirm.Select.nowait = false } >>= fun () ->
  return return_handler

let register_return_handler t =
  let (_, read) = Basic.Return.Internal.read in
  let handler m = List.iter (fun h -> h (Some m)) t.return_handlers in
  read ~once:false handler (channel t)


let create: type a. id:string -> a confirms -> Framing.t -> Framing.channel_no -> a t Deferred.t = fun ~id confirm_type framing channel_no ->
  let consumers = Hashtbl.create 0 in
  let id = Printf.sprintf "%s.%s.%d" (Framing.id framing) id channel_no in
  Framing.open_channel framing channel_no >>= fun () ->
  let publish_confirm : a pcp = match confirm_type with
    | With_confirm ->
      Pcp_with_confirm { message_count = 0; unacked = Mlist.create () }
    | No_confirm ->
      Pcp_no_confirm
  in
  let t =
    { framing; channel_no; consumers; id; counter = 0;
      publish_confirm; return_handlers = []; closed = None; }
  in

  spawn (Channel.Close.reply (framing, channel_no) (close_handler t channel_no));
  let (_, read) = Basic.Cancel.Internal.read in
  read ~once:false (consumer_cancel_handler t) (framing, channel_no);

  Channel.Open.request (framing, channel_no) () >>= fun () ->

  begin match publish_confirm with
  | Pcp_with_confirm t ->
    handle_confirms (framing, channel_no) t >>= fun return_handler ->
    return [return_handler]
  | Pcp_no_confirm ->
    return []
  end >>= fun return_handlers ->
  t.return_handlers <- return_handlers;
  Internal.register_deliver_handler t;

  register_flow_handler t;
  register_return_handler t;
  return t

let close { framing; channel_no; return_handlers; _ } =
  let open Channel.Close in
  request (framing, channel_no)
    { reply_code=200;
      reply_text="Closed on user request";
      class_id=0;
      method_id=0; } >>= fun () ->
  Framing.close_channel framing channel_no >>= fun () ->
  List.iter (fun h -> h None) return_handlers;
  return ()

let on_return t =
  let reader, writer = Pipe.create () in
  let handler = function
    | Some m -> Pipe.write_without_pushback writer m
    | None -> Pipe.close_without_pushback writer
  in
  t.return_handlers <- handler :: t.return_handlers;
  reader

let on_closed t =
  let ivar =
    match t.closed with
    | Some ivar ->
      ivar
    | None ->
      let ivar = Ivar.create () in
      t.closed <- Some ivar;
      ivar
  in
  Ivar.read ivar

let flush t =
  Framing.flush_channel t.framing t.channel_no

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

module Transaction = struct
  type tx = EChannel: _ t -> tx

  open Spec.Tx
  let start t =
    Select.request (channel t) () >>= fun () ->
    return (EChannel t)

  let commit (EChannel t) =
    Commit.request (channel t) ()

  let rollback (EChannel t) =
    Rollback.request (channel t) ()
end

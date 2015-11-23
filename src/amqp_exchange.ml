open Async.Std
module Connection = Amqp_connection
module Channel = Amqp_channel

open Amqp_spec.Exchange

(* type match_type = Any | All *)

type _ exchange_type =
  | Direct: (queue:string -> unit Deferred.t) exchange_type
  | Fanout: (unit Deferred.t) exchange_type
  | Topic: (topic:string -> unit Deferred.t) exchange_type
  | Match: (headers:(Amqp_types.header list) -> unit Deferred.t) exchange_type

let direct_t = Direct
let fanout_t = Fanout
let topic_t = Topic
let match_t = Match

type 'a t = { name : string;
              exchange_type: 'a exchange_type }

(** Predefined Default exchange *)
let default    = { name=""; exchange_type = Direct }

(** Predefined Direct exchange *)
let amq_direct = { name = "amq.direct"; exchange_type = Direct }

(** Predefined Fanout exchange *)
let amq_fanout = { name = "amq.fanout";  exchange_type = Fanout }

(** Predefined topic exchange *)
let amq_topic  = { name = "amq.topic"; exchange_type = Topic }

(** Predefined match (header) exchange *)
let amq_match = { name = "amq.match"; exchange_type = Match }

let string_of_exchange_type: type a. a exchange_type -> string  = function
  | Direct -> "direct"
  | Fanout -> "fanout"
  | Topic -> "topic"
  | Match -> "match"

module Internal = struct
  let bind_queue: type a. _ Channel.t -> a t -> string -> a =
    let open Amqp_spec.Queue in
    fun channel { name; exchange_type} queue ->
      let bind ?(routing_key="") ?(arguments=[]) () =
        let query = { Bind.queue;
                      exchange = name;
                      routing_key;
                      no_wait = false;
                      arguments;
                    }
        in
        Bind.request (Channel.channel channel) query
      in
      match exchange_type with
      | Direct -> fun ~queue:routing_key -> bind ~routing_key ()
      | Fanout -> bind ()
      | Topic -> fun ~topic:routing_key -> bind ~routing_key ()
      | Match -> fun ~headers:arguments -> bind ~arguments ()

  let unbind_queue: type a. _ Channel.t -> a t -> string -> a =
    let open Amqp_spec.Queue in
    fun channel { name; exchange_type} queue ->
      let unbind ?(routing_key="") ?(arguments=[]) () =
        let query = { Unbind.queue;
                      exchange = name;
                      routing_key;
                      arguments;
                    }
        in
        Unbind.request (Channel.channel channel) query
      in
      match exchange_type with
      | Direct -> fun ~queue:routing_key -> unbind ~routing_key ()
      | Fanout -> unbind ()
      | Topic -> fun ~topic:routing_key -> unbind ~routing_key ()
      | Match -> fun ~headers:arguments -> unbind ~arguments ()

end


let declare: type a. ?passive:bool -> ?durable:bool -> ?auto_delete:bool -> _ Channel.t -> a exchange_type -> string -> a t Deferred.t =
  fun ?(passive=false) ?(durable=false) ?(auto_delete=false) channel exchange_type name ->
    Declare.request (Channel.channel channel)
      { Declare.exchange = name;
        amqp_type = (string_of_exchange_type exchange_type);
        passive;
        durable;
        auto_delete;
        internal = false;
        no_wait = false;
        arguments = [] } >>= fun () ->
    return { name; exchange_type }

let delete ?(if_unused=false) channel t =
  Delete.request (Channel.channel channel)
    { Delete.exchange = t.name;
      if_unused;
      no_wait = false;
    }

let bind: type a. _ Channel.t -> destination:_ t -> source:a t -> a =
  fun channel ~destination ~source ->
    let bind ?(routing_key="") ?(arguments=[]) () =
      let query = { Bind.destination = destination.name;
                    source = source.name;
                    routing_key;
                    no_wait = false;
                    arguments;
                  }
      in
      Bind.request (Channel.channel channel) query
    in
    match source.exchange_type with
    | Direct -> fun ~queue:routing_key -> bind ~routing_key ()
    | Fanout -> bind ()
    | Topic -> fun ~topic:routing_key -> bind ~routing_key ()
    | Match -> fun ~headers:arguments -> bind ~arguments ()


let unbind: type a. _ Channel.t -> destination:_ t -> source:a t -> a =
  fun channel ~destination ~source ->
    let unbind ?(routing_key="") ?(arguments=[]) () =
      let query = { Unbind.destination = destination.name;
                    source = source.name;
                    routing_key;
                    no_wait = false;
                    arguments;
                  }
      in
      Unbind.request (Channel.channel channel) query
    in
    match source.exchange_type with
      | Direct -> fun ~queue:routing_key -> unbind ~routing_key ()
      | Fanout -> unbind ()
      | Topic -> fun ~topic:routing_key -> unbind ~routing_key ()
      | Match -> fun ~headers:arguments -> unbind ~arguments ()

let publish channel t
    ?(mandatory=false)
    ~routing_key
    (header, body) =

  let open Amqp_spec.Basic in
  let header = match header.Content.app_id with
    | Some _ -> header
    | None -> { header with Content.app_id = Some (Amqp_channel.id channel) }
  in
  let wait_for_confirm = Channel.Internal.wait_for_confirm channel in
  Publish.request (Amqp_channel.channel channel)
    ({Publish.exchange = t.name;
      routing_key=routing_key;
      mandatory;
      immediate=false},
     header, body) >>= fun () ->
  wait_for_confirm

let name t = t.name

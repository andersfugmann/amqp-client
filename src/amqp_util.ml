open Async.Std
open Amqp_types
open Amqp_protocol

type 'a post_handler = ('a -> unit) option

let bit_string v length =
  let rec loop acc v = function
    | 0 -> acc
    | n -> loop ((if v land 1 = 1 then "1" else "0") :: acc) (v lsr 1) (n-1)
  in
  String.concat "" (loop [] v length)

let update_property_flag v word flags =
  word ((v lsl (16 - flags)) land 0xffff)

let read_property_flag word flags =
  let word = (word land 0xffff) lsr (16 - flags) in

  (* Reverse the bits *)
  let rec rev v word = function
    | 0 -> v
    | n ->
      rev ((v lsl 1) lor (word land 0x1)) (word lsr 1) (n - 1)
  in
  rev 0 word flags

let rec list_create f = function
  | 0 -> []
  | n -> f () :: list_create f (n - 1)

let write_method (message_id, spec, _make, apply) =
  let write = Spec.write spec in
  let request channel msg =
    let data =
      apply (write (Output.create ())) msg
    in
    Amqp_framing.write_method channel message_id data
  in
  request

let read_method (message_id, spec, make, _apply) =
  let read = Spec.read spec in
  let reply ?(post_handler : 'a post_handler) channel =
    let var = Ivar.create () in
    let handler data =
      let req = read make data in
      Ivar.fill var req;
      Amqp_framing.deregister_method_handler channel message_id;
      match post_handler with Some h -> h req | None -> ()
    in
    Amqp_framing.register_method_handler channel message_id handler;
    Ivar.read var
  in
  (message_id, reply)

let write_method_content (message_id, spec, _make, apply) ((c_method, _), c_spec, _c_make, c_apply) =
  let write = Spec.write spec in
  let c_write = Content.write c_spec in
  let property_bits = Content.length c_spec in
  assert (property_bits <= 15);

  fun channel (meth, content, data) ->
    apply (write (Output.create ())) meth |> Amqp_framing.write_method channel message_id;
    let output = Output.create () in
    let property_flags = ref 0 in
    let property_word = Output.short_ref output in
    c_apply (c_write property_flags output) content;
    update_property_flag !property_flags property_word property_bits;

    (* Now send the data. *)
    Amqp_framing.write_content channel c_method output data

let read_method_content (message_id, spec, make, _apply) ((c_method, _), c_spec, c_make, _c_apply) =
  let read = Spec.read spec in
  let c_read = Content.read c_spec in
  let flags = Content.length c_spec in

  let reply ?(post_handler : 'a post_handler) channel =
    let var = Ivar.create () in
    let c_handler req (content, data) =
      let property_flags = read_property_flag (Input.short content) flags in
      let header = c_read c_make property_flags content in
      let message = (req, (header, data)) in
      Ivar.fill var message;
      Amqp_framing.deregister_content_handler channel c_method;
      match post_handler with Some h -> h message | None -> ()
    in
    let handler data =
      let req = read make data in
      Amqp_framing.deregister_method_handler channel message_id;
      Amqp_framing.register_content_handler channel c_method (c_handler req);
    in
    Amqp_framing.register_method_handler channel message_id handler;
    Ivar.read var
  in
  (message_id, reply)

let request0 req =
  fun channel msg ->
    req channel msg;
    return ()

let reply0 (_, rep) =
  fun ?post_handler channel -> rep ?post_handler channel

let request1 req (_, rep) =
  fun ?(post_handler : 'a post_handler) channel msg ->
    req channel msg;
    rep ?post_handler channel

let reply1 (_, rep) req =
  fun ?post_handler channel (handler : 'a -> 'b Deferred.t) ->
    rep ?post_handler channel >>= handler >>= fun msg ->
    req channel msg;
    return ()

let request2 req (mid1, rep1) id1 (mid2, rep2) id2 =
  let unregister channel mid _ =
    Amqp_framing.deregister_method_handler channel mid
  in
  fun channel msg ->
    req channel msg;
    (* Choose either one *)
    Deferred.any [
      rep1 ?post_handler:(Some (unregister channel mid2)) channel >>= (fun a -> return (id1 a));
      rep2 ?post_handler:(Some (unregister channel mid1)) channel >>= (fun a -> return (id2 a));
    ]

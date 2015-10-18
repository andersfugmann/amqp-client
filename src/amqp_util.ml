module P = Printf
open Async.Std
open Amqp_types
open Amqp_protocol

let bit_string v length =
  let rec loop acc v = function
    | 0 -> acc
    | n -> loop ((if v land 1 = 1 then "1" else "0") :: acc) (v lsr 1) (n-1)
  in
  String.concat "" (loop [] v length)

let update_property_flags _bits v words =
  (* TODO: If there are more than 15 fields, this function may not work. *)
  assert (List.length words = 1);
  (* let v = v lsr ((List.length words) * 15 - bits) in *)
  let rec write first v = function
    | f :: xs ->
      let p = ((v land 0x7fff) * 2 + (if first then 0 else 1)) in
      log "Write property flags: %x" p;
      f p;
      write false (v lsr 15) xs
    | [] -> ()
  in
  write true v (List.rev words)

let read_property_flags input =
  let rec add_flags v flags = function
    | 0 -> v
    | n ->
      add_flags ((v lsl 1) lor (flags land 0x1)) (flags lsr 1) (n - 1)
  in
  let rec read_property_words v input =
    let flags = (decode Short input) land 0xffff in
    log "Read flags: %x" (flags land 0xffff);
    log "Read flags: %x" flags;
    let v = add_flags v (flags lsr 1) 15 in
    if flags mod 2 = 1 then
      read_property_words v input
    else
      v
  in
  read_property_words 0 input


let rec list_create f = function
  | 0 -> []
  | n -> f () :: list_create f (n - 1)

let write_content ((cid, _), spec, _make, apply) =
  let write = Content.write spec in
  let property_bits = Content.length spec in
  let property_length = (property_bits + 14) / 15 in
  let property_flags = ref 0 in
  fun channel (content, message) ->
    let output = Output.create () in
    let property_words = list_create (fun () -> Output.short_ref output) property_length in
    apply (write property_flags output) content;
    update_property_flags property_bits !property_flags property_words;

    (* Now send the data. *)
    Amqp_channel.write_content channel cid output message


let read_content ((cid, _), spec, make, _apply) =
  let read = Content.read spec in
  fun channel ->
    let var = Ivar.create () in
    let handler (content, data) =
      (* Read in all property flags *)
      let property_flags = read_property_flags content in
      let header = read make property_flags content in
      Ivar.fill var (header, data);
      Amqp_channel.remove_content_handler channel cid;
    in
    Amqp_channel.add_content_handler channel cid handler;
    Ivar.read var

let write_method (message_id, spec, _make, apply) =
  let write = Spec.write spec in
  let request channel msg =
    let data =
      apply (write (Output.create ())) msg
    in
    Amqp_channel.write_method channel message_id data
  in
  request

let read_method (message_id, spec, make, _apply) =
  let read = Spec.read spec in
  let reply post_handler channel =
    let var = Ivar.create () in
    let handler data =
      let req = read make data in
      Ivar.fill var req;
      Amqp_channel.remove_method_handler channel message_id;
      post_handler channel
    in
    Amqp_channel.add_method_handler channel message_id handler;
    Ivar.read var
  in
  (message_id, reply)

let write_method_content req req_content =
  fun channel (message, content, data) ->
    req channel message;
    req_content channel (content, data)

let read_method_content (message_id, rep) rep_content =
  let rep post_handler channel =
    rep post_handler channel >>= fun a ->
    rep_content channel >>= fun b ->
    return (a, b)
  in
  (message_id, rep)

let request0 req =
  fun channel msg ->
    req channel msg;
    Amqp_channel.flush channel

let reply0 (_, rep) = rep

let request1 req (_, rep) =
  fun channel msg ->
    req channel msg;
    Amqp_channel.flush channel >>= fun () ->
    rep ignore channel

let reply1 (_, rep) req =
  fun channel (handler : 'a -> 'b Deferred.t) ->
    rep ignore channel >>= handler >>= fun msg ->
    req channel msg;
    Amqp_channel.flush channel

let request2 req (mid1, rep1) id1 (mid2, rep2) id2 =
  let unregister mid channel =
    Amqp_channel.remove_method_handler channel mid
  in
  fun channel msg ->
    req channel msg;
    Amqp_channel.flush channel >>= fun () ->
    (* Choose either one *)
    Deferred.any [
      rep1 (unregister mid2) channel >>= (fun a -> return (id1 a));
      rep2 (unregister mid1) channel >>= (fun a -> return (id2 a));
    ]

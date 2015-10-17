open Async.Std
open Amqp_types
open Amqp_protocol

let log fmt = printf (fmt ^^ "\n%!")

(*
Bits are pushed from the bottom - Oh, I need the bits when done
So if only the first is set, it must become the 16 bit. (I hate them!)
What if i convert it little endian???

*)

let update_property_flags bits v words =
  let v = v lsl ((List.length words) * 15 - bits) in
  let rec write first v = function
    | f :: xs ->
      f ((v land 0x7f) lsl 1 + (if first then 0 else 1));
      write false (v lsr 15) xs
    | [] -> ()
  in
  write true v (List.rev words)

let read_property_flags input =
  let rec add_flags v flags = function
    | 0 -> v
    | n ->
      add_flags ((v * 2) + (flags mod 2)) (flags / 2) (n - 1)
  in
  let rec read_property_words v input =
    let flags = Input.short input in
    let v = add_flags v (flags / 2) 15 in
    if flags mod 2 = 1 then
      read_property_words v input
    else
      v
  in
  read_property_words 0 input

let rec list_create f = function
  | 0 -> []
  | n -> f () :: list_create f (n - 1)

let write_content (message_id, spec, _make, apply) =
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
    Amqp_channel.write_content channel (fst message_id) output message


let read_content ((cid, _), spec, make, _apply) =
  let read = Content.read spec in
  let var = Ivar.create () in

  fun channel ->
    let handler (content, data) =
      (* Read in all property flags *)
      let property_flags = read_property_flags content in
      let header = read make property_flags content in
      Ivar.fill var (header, data);
      Amqp_channel.remove_content_handler channel cid;
    in
    Amqp_channel.add_content_handler channel cid handler;
    Ivar.read var


let request (message_id, spec, _make, apply) =
  let write = Spec.write spec in
  let request channel msg =
    let data =
      apply (write (Output.create ())) msg
    in
    Amqp_channel.write_method channel message_id data
  in
  request

let request_content (message_id, spec, _make, apply) c_req =
  let write = Spec.write spec in
  let request channel (msg, content) =
    apply (write (Output.create ())) msg
    |> Amqp_channel.write_method channel message_id;
    c_req channel content
  in
  request

let reply (message_id, spec, make, _apply) =
  let read = Spec.read spec in
  let var = Ivar.create () in
  let reply post_handler channel =
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

let reply_content spec (_, c_rep) =
  let tpe, reply = reply spec in
  let reply post_handler channel =
    reply post_handler channel >>= fun m ->
    c_rep ignore channel >>= fun c ->
    return (m, c)
  in
  (tpe, reply)

let request0 req = req

let reply0 (_, rep) = rep

let request1 req (_, rep) =
  fun channel msg ->
    req channel msg;
    rep ignore channel

let reply1 (_, rep) req =
  fun channel (handler : 'a -> 'b Deferred.t) ->
    rep ignore channel >>= handler >>= fun msg ->
    req channel msg;
    return ()

let request2 req (mid1, rep1) id1 (mid2, rep2) id2 =
  let unregister mid channel =
    Amqp_channel.remove_method_handler channel mid
  in
  fun channel msg ->
    req channel msg;
    (* Choose either one *)
    Deferred.any [
      rep1 (unregister mid2) channel >>= (fun a -> return (id1 a));
      rep2 (unregister mid1) channel >>= (fun a -> return (id2 a));
    ]

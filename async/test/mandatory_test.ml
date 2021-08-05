open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let assert_returned_message reader body =
  Pipe.read reader >>| function
  | `Ok (_, (_, m)) -> assert (m = body); ()
  | `Eof -> assert false

let assert_reader_closed reader =
  Pipe.read reader >>| function
  | `Ok _ -> assert false
  | `Eof -> ()

let print_r = function
  | `Ok -> Printf.eprintf "Got ok\n%!"
  | `Failed -> Printf.eprintf "Got failed\n%!"

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.with_confirm connection >>= fun channel ->
  Log.info "Channel opened";

  Exchange.publish channel Exchange.amq_direct ~mandatory:false ~routing_key:"non_existant_queue" (Message.make "") >>= fun r ->
  assert (r = `Ok);
  Exchange.publish channel Exchange.amq_direct ~mandatory:true ~routing_key:"non_existant_queue" (Message.make "") >>= fun r ->
  assert (r = `Failed);

  (* Test on_return delivery handler *)
  let reader1 = Channel.on_return channel in
  let reader2 = Channel.on_return channel in
  let body = "Return this message" in
  Exchange.publish channel Exchange.amq_direct ~mandatory:true ~routing_key:"non_existant_queue" (Message.make body) >>= fun r ->
  assert (r = `Failed);
  assert_returned_message reader1 body >>= fun () ->
  assert_returned_message reader2 body >>= fun () ->

  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  assert_reader_closed reader1 >>= fun () ->
  assert_reader_closed reader2 >>= fun () ->

  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

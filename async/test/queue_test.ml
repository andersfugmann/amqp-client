open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:true (uniq "queue.test") >>= fun queue ->
  Log.info "Queue declared";
  Channel.set_prefetch channel ~count:100 >>= fun () ->
  Log.info "Prefetch set";
  Queue.purge channel queue >>= fun () ->
  Log.info "Queue purged";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  assert (m = None);
  Log.info "Queue empty";
  Queue.publish channel queue (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  Log.info "Message published";
  Channel.flush channel >>= fun () ->
  Log.info "Channel flushed";

  Queue.get ~no_ack:false channel queue >>= fun m ->
  let m = match m with
    | None -> failwith "No message"
    | Some m -> m
  in
  Log.info "Message received";
  Message.ack channel m >>= fun () ->

  Exchange.declare channel Exchange.topic_t (uniq "test_exchange") >>= fun exchange ->
  Log.info "Exchange declared";
  Queue.bind channel queue exchange (`Topic (uniq "test.#.key")) >>= fun () ->
  Log.info "Queue bind declared";

  Exchange.publish channel exchange ~routing_key:(uniq "test.a.b.c.key") (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  Log.info "Message published";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  let m = match m with
    | None -> failwith "No message"
    | Some m -> m
  in
  Log.info "Message recieved";
  Message.ack channel m >>= fun () ->
  Queue.delete channel queue >>= fun () ->
  Log.info "Queue deleted";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

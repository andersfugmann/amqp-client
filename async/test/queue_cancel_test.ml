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
  (* Start consuming *)
  let cancelled = ref false in
  Queue.consume ~id:(uniq "consume_test") ~on_cancel:(fun () -> cancelled := true) channel queue >>= fun (_consumer, reader) ->
  Queue.publish channel queue (Message.make "Test") >>= fun `Ok ->
  Pipe.read reader >>= fun res ->
  assert (res <> `Eof);
  Log.info "Message read";

  (* Delete the queue *)
  Queue.delete channel queue >>= fun () ->
  Log.info "Queue deleted";
  Pipe.read reader >>= fun res ->
  assert (res = `Eof);
  assert (!cancelled);
  Log.info "Consumer cancelled";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

let () = Printf.printf "Done\n"

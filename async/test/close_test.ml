open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let test () =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~virtual_host:"/" ~id:(uniq "ocaml-amqp-test") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:false (uniq "queue.test") >>= fun queue ->
  Log.info "Queue declared";
  Queue.publish channel queue (Message.make "Test") >>= fun res ->
  assert (res = `Ok);
  Log.info "Message published";
  Connection.close connection >>= fun () ->
  Log.info "Connection closed";
  Connection.connect ~virtual_host:"/" ~id:(uniq "ocaml-amqp-test") "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:false (uniq "queue.test") >>= fun queue ->
  Log.info "Queue declared";
  Queue.get ~no_ack:false channel queue >>= fun m ->
  (match m with
    | None -> failwith "No message"
    | Some _ -> ()
  );
  Log.info "Message received";
  Queue.delete channel queue >>= fun () ->
  Log.info "Queue deleted";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  test () |> ignore;
  Scheduler.go ()
let () = Printf.printf "Done\n"

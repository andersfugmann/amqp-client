open Amqp_client_async
open Thread

(* Demonstrate how on_close can be used *)

let _ =
  let _ =
    Connection.connect ~id:"fugmann" ~heartbeat:10 "localhost" >>= fun connection ->
    spawn (Connection.on_closed connection >>= fun () ->
           Log.info "Connection closed";
           return ());

    Log.info "Connection started.";
    Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
    Log.info "Channel opened";
    spawn (Channel.on_closed channel >>= fun () ->
           Log.info "Channel closed - Handler";
           return ());

    spawn (Connection.on_closed connection >>= fun () ->
           Log.info "Connection closed - Handler";
           return ());
    Unix.sleep 300;
    (*
    Connection.close connection >>= fun () ->
    Log.info "Connection closed ";
*)
    Log.info "Done";
    return ();
  in
  Scheduler.go ()

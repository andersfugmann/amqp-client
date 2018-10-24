open Amqp_client_async
open Thread

(* Demonstrate how on_close can be used *)

let _ =
  let _ =
    Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
    spawn (Connection.on_closed connection >>= fun () ->
           Log.info "Connection closed";
           return ());

    Log.info "Connection started";
    Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
    Log.info "Channel opened";
    spawn (Channel.on_closed channel >>= fun () ->
           Log.info "Channel closed";
           return ());
    return ();
  in
  Scheduler.go ()

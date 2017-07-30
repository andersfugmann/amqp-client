open Amqp
open Amqp_thread

let handler (h, s) =
  Log.info "Recieved request: %s" s;
  return (h, s)

let start =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~arguments:[Rpc.Server.queue_argument] "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Server.start channel queue handler >>= fun _server ->
  Log.info "Listening for requsts";

  return ()

let _ =
  Scheduler.go ()

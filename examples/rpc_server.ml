open Async
open Amqp_client_async

let handler (h, s) =
  Log.Global.info "Recieved request: %s" s;
  return (h, s)

let start =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.Global.info "Connection started";
  Connection.open_channel ~id:"test" Channel.no_confirm connection >>= fun channel ->
  Log.Global.info "Channel opened";
  Queue.declare channel ~arguments:[Rpc.Server.queue_argument] "rpc.server.echo_reply" >>= fun queue ->
  Rpc.Server.start channel queue handler >>= fun _server ->
  Log.Global.info "Listening for requsts";

  return ()

let _ =
  Scheduler.go ()

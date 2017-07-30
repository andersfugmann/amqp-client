open Amqp
open Amqp_thread


let rec repeat channel queue =
  Log.info "rep";
  Queue.publish channel queue (Message.make "Test") >>= function
  | `Ok ->
      begin
        Queue.get ~no_ack:true channel queue >>= function
        | Some _ ->
            after 1000.0 >>= fun () ->
            repeat channel queue
        | None -> failwith "No message"
      end
  | _ -> failwith "Cannot publish"

let test =
  Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"test.repeat" Channel.no_confirm connection >>= fun channel ->
  Queue.declare channel ~auto_delete:true "test.repeat" >>= fun queue ->
  repeat channel queue >>= fun () ->
  Connection.close connection >>= fun () ->
  Scheduler.shutdown 0 |> return

let _ =
  Scheduler.go ()

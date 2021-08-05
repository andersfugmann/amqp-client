open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

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
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "test.repeat") Channel.no_confirm connection >>= fun channel ->
  Queue.declare channel ~auto_delete:true (uniq "test.repeat") >>= fun queue ->
  repeat channel queue >>= fun () ->
  Connection.close connection >>= fun () ->
  Scheduler.shutdown 0 |> return

let _ =
  Scheduler.go ()

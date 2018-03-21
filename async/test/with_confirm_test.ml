open Amqp
open Amqp.Thread

let rec consume_queue channel queue =
  Queue.get ~no_ack:true channel queue >>= function
  | Some _ ->
      consume_queue channel queue >>| ((+) 1)
  | None -> return 0

let rec list_create = function
  | 0 -> []
  | n -> n :: list_create (n - 1)

let test =
  Connection.connect ~id:"inteegration_test" "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:"with_confirm.test" Channel.with_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Queue.declare channel ~auto_delete:true "with_confirm_test" >>= fun queue ->
  Queue.purge channel queue >>= fun () ->

  (* Publish 1000 messages in one go, and wait for all of them to complete *)
  let messages = 1000 in
  list_create messages
  |> Deferred.List.iter ~how:`Parallel ~f:(fun i -> Queue.publish channel queue (Message.make (string_of_int i)) >>| ignore)
  >>= fun () ->

  consume_queue channel queue >>= fun message_count ->
  assert (message_count = messages);

  Queue.delete channel queue >>= fun () ->

  Channel.close channel >>= fun () ->
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()

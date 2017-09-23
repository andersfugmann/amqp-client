open Amqp
open Amqp.Thread

let consume channel queue =
  let handler { Message.message = (_content, body); _ } =
    let i = int_of_string body in
    begin match i with
      | i when i mod 1000 = 0 ->
        Log.info "%i" i
      | 1 ->
        Log.info "Done";
        Scheduler.shutdown 0
      | _ -> ()
    end;
  in
  Queue.consume ~no_ack:true ~id:"test" channel queue >>= fun (_consumer, reader) ->
  spawn (Pipe.iter_without_pushback reader ~f:(fun m -> handler m));
  return ()

let rec produce channel queue = function
  | 0 -> Channel.flush channel;
  | n ->
    Queue.publish channel queue (Message.make (string_of_int n)) >>= fun res ->
    assert (res = `Ok);
    produce channel queue (n-1)

let _ =
  let _ =
    Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
    Log.info "Connection started";
    Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
    Log.info "Channel opened";
    Queue.declare channel ~arguments:[] ~auto_delete:true "test.main" >>= fun queue ->

    spawn (consume channel queue);
    produce channel queue 50000 >>= fun () ->
    Log.info "Done producing";
    return ();
  in
  Scheduler.go ()

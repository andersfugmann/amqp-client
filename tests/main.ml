open Amqp
open Amqp_thread

let log fmt = Printf.printf (fmt ^^ "\n%!")

let consume channel queue =
  let handler { Message.message = (_content, body); _ } =
    let i = int_of_string body in
    begin match i with
      | i when i mod 1000 = 0 ->
        log "%i" i
      | 1 ->
        log "Done";
        Scheduler.shutdown 0
      | _ -> ()
    end;
    return ()
  in
  Queue.consume ~no_ack:true ~id:"test" channel queue >>= fun (_consumer, reader) ->
  spawn (Pipe.iter_without_pushback reader ~f:(fun m -> spawn (handler m)));
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
    log "Connection started";
    Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
    log "Channel opened";
    Queue.declare channel ~arguments:[] ~auto_delete:true "test.main" >>= fun queue ->

    spawn (consume channel queue);
    produce channel queue 100000 >>= fun () ->
    log "Done producing";
    return ();
  in
  Scheduler.go ()

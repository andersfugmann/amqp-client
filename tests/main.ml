open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let rec sync_loop channel queue i =
  Queue.publish channel queue (Message.make (string_of_int i)) >>= fun res ->
  assert (res = `Ok);
  Queue.get ~no_ack:false channel queue >>= function
  | Some ({ Message.message = (_, body); _ } as msg) ->
    log "Received: %s" body;
    Message.ack channel msg >>= fun () ->
    sync_loop channel queue (i+1)
  | None -> failwith "No message"

let consume channel queue =
  let handler { Message.message = (_content, body); _ } =
    let i = int_of_string body in
    begin match i with
      | i when i mod 1000 = 0 ->
        log "%i" i
      | 1 ->
        log "Done";
        Shutdown.shutdown 0
      | _ -> ()
    end;
    return ()
  in
  Queue.consume ~no_ack:true ~id:"test" channel queue >>= fun (_consumer, reader) ->
  don't_wait_for (Pipe.iter_without_pushback reader ~f:(fun m -> don't_wait_for (handler m)));
  return ()

let rec produce channel queue = function
  | 0 -> return ();
  | n ->
    don't_wait_for (Queue.publish channel queue (Message.make (string_of_int n)) >>= fun res -> assert (res = `Ok); return ());
    produce channel queue (n-1)

let _ =
  let _ =
    Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
    log "Connection started";
    Connection.open_channel Channel.no_confirm ~id:"test" connection >>= fun channel ->
    log "Channel opened";
    Queue.declare channel ~arguments:[] ~auto_delete:true "test.main" >>= fun queue ->

    (* sync_loop channel queue 1 *)
    don't_wait_for (consume channel queue);
    produce channel queue 100000 >>= fun () ->
    log "Done producing";
    return ();
  in
  Scheduler.go ()

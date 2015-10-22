open Async.Std
open Amqp

let log fmt = printf (fmt ^^ "\n%!")

let rec sync_loop channel queue i =
  Queue.publish channel queue (Printf.sprintf "Message: %d" i) >>= fun () ->
  Queue.get ~no_ack:false channel queue (fun _ _ msg -> log "Received: %s" msg; return ()) >>= fun () ->
  sync_loop channel queue (i+1)

let consume channel queue =
  let handler _ _ msg =
    let i = Scanf.sscanf msg "Message: %d" (fun i -> i) in
    begin match i with
      | i when i mod 1000 = 0 ->
        log "%i" i
      | 1 -> log "Done"
      | _ -> ()
    end;
    return ()
  in
  Queue.consume channel queue handler >>= fun _stop ->
  return ()

let rec produce channel queue = function
  | 0 -> return ();
  | n -> Queue.publish channel queue ~message_id:"vilde dyr" (Printf.sprintf "Message: %d" n) >>= fun () ->
    produce channel queue (n-1)

let _ =
  let _ =
    Connection.connect ~id:"fugmann" "localhost" >>= fun connection ->
    log "Connection started";
    Connection.open_channel ~id:"test" connection >>= fun channel ->
    log "Channel opened";
    let arguments =
      let open Queue in
      [ maximum_priority 7 ]
    in
    Queue.declare channel ~arguments "anders" >>= fun queue ->
    (* sync_loop channel queue 1 *)
    don't_wait_for (consume channel queue);
    produce channel queue 100000 >>= fun () ->
    log "Done producing";
    return ();
  in
  Scheduler.go ()

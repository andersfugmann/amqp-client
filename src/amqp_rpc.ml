open Async.Std

module Client = struct
  (* Each client should have its own channel *)
  (* We then register correlation-id and have callers wait for an Ivar.
     If we are running rabbitmq, we do not need to create a new queue, but can use amq.rabbit.reply-to
  *)
  type t = unit
  let init connection = ignore connection

  let call ~ttl queue request =
    (* Handle ttl, so we need to setup a return if the message could not be delivered in due time *)
    (* Also, we dont want to wait forever.... I dont know if we care here though *)
    ignore (queue, request, ttl);
    return ()

  (** Release resources *)
  let close t = ignore t; return ()
end

module Server = struct
  (* We need to define a queue, and listen receive on this queue *)
  type t = unit
  let init channel queue handler =
    ignore (channel, queue, handler);
    return t

  let stop t = ignore t; return ()
end

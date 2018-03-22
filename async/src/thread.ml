(** Async compatibility layer *)
open Amqp_client_lib
open Async

module Deferred = struct
  type 'a t = 'a Deferred.t
  let all_unit = Deferred.all_unit
  let try_with f = Monitor.try_with ~extract_exn:true f >>= function
    | Core.Result.Ok v -> return (`Ok v)
    | Core.Result.Error exn -> return (`Error exn)
  module List = struct
    let init ~f n = Deferred.List.init ~f n
    let iter ?(how:[`Sequential | `Parallel] = `Parallel) ~f l = Deferred.List.iter ~how:(how :> Async_kernel.Monad_sequence.how) ~f l
  end

end

let (>>=) = (>>=)
let (>>|) = (>>|)
let return a = return a
let after ms = after (Core.Time.Span.of_ms ms)
let spawn t = don't_wait_for t

let with_timeout seconds deferred =
  let duration = Core.Time.Span.of_sec (float_of_int seconds) in
  Clock.with_timeout duration deferred

module Ivar = struct
  type 'a t = 'a Ivar.t
  let create = Ivar.create
  let create_full = Ivar.create_full
  let fill = Ivar.fill
  let read t = Ivar.read t
  let is_full = Ivar.is_full
  let fill_if_empty = Ivar.fill_if_empty
end

module Reader = struct
  type t = Reader.t
  let close = Reader.close
  let read t buf = Reader.really_read t buf
end

module Writer = struct
  type t = Writer.t
  let write t buf = Writer.write t buf
  let close t = Writer.close t
  let flush t = Writer.flushed t
end

module Tcp = struct
  let connect ?nodelay host port =
    let addr = Core.Host_and_port.create ~host ~port
               |> Tcp.Where_to_connect.of_host_and_port
    in
    Tcp.connect ~buffer_age_limit:`Unlimited addr >>= fun (s, r, w) ->
    (match nodelay with
     | Some () -> Socket.setopt s Socket.Opt.nodelay true
     | None -> ());
    return (r, w)
end

module Log = struct
  (* Use of a predefiend tag allows the caller to disable logging if needed *)
  let tags = ["library", "amqp_client"]
  let debug fmt = Log.Global.debug ~tags fmt
  let info fmt = Log.Global.info ~tags fmt
  let error fmt = Log.Global.error ~tags fmt
end

(* Pipes *)
module Pipe = struct
  module Writer = struct
    type 'a t = 'a Pipe.Writer.t
  end
  module Reader = struct
    type 'a t = 'a Pipe.Reader.t
  end

  let create () = Pipe.create ()
  let set_size_budget t = Pipe.set_size_budget t
  let flush t = Pipe.downstream_flushed t >>= fun _ -> return ()
  let interleave_pipe t = Pipe.interleave_pipe t
  let write r elm = Pipe.write r elm
  let write_without_pushback r elm = Pipe.write_without_pushback r elm

  let transfer_in ~from t =
    Ocaml_lib.Queue.iter (write_without_pushback t) from;
    return ()

  let close_without_pushback t = Pipe.close t
  let close t = Pipe.close t; flush t >>= fun _ -> return ()
  let read r = Pipe.read r
  let iter r ~f = Pipe.iter r ~f
  let iter_without_pushback r ~f = Pipe.iter_without_pushback r ~f

end

module Scheduler = struct
  let go () = Scheduler.go () |> ignore
  let shutdown n = Shutdown.shutdown n
end

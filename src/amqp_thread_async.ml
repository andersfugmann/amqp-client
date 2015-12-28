(** Async compatability layer *)

open Async.Std

module Deferred = struct
  type 'a t = 'a Deferred.t
  let all_unit = Deferred.all_unit
  module List = struct
    let init ~f n = Deferred.List.init ~f n
    let iter ~f l = Deferred.List.iter ~f l
  end
end

let (>>=) = (>>=)
let (>>|) = (>>|)
let return = return
let after ms = after (Core.Std.Time.Span.of_ms ms)
let spawn = don't_wait_for

module Ivar = struct
  type 'a t = 'a Ivar.t
  let create = Ivar.create
  let create_full = Ivar.create_full
  let fill = Ivar.fill
  let read = Ivar.read
  let is_full = Ivar.is_full
  let fill_if_empty = Ivar.fill_if_empty
end

module Tcp = struct
  let connect host port =
    let addr = Tcp.to_host_and_port host port in
    Tcp.connect addr

  let nodelay socket value =
    Socket.setopt socket Socket.Opt.nodelay value

end
module Reader = struct
  type t = Reader.t
  let close = Reader.close
  let really_read = Reader.really_read
end

module Writer = struct
  type t = Writer.t
  let write = Writer.write
  let close = Writer.close
  let flush = Writer.flushed
end


(* Pipes *)
module Pipe = struct
  type ('a, 'phantom) t = ('a, 'phantom) Pipe.t
  let create = Pipe.create
  let set_size_budget = Pipe.set_size_budget
  let flush t = Pipe.downstream_flushed t
  let interleave_pipe = Pipe.interleave_pipe
  let write = Pipe.write
  let write_without_pushback = Pipe.write_without_pushback

  let transfer_in ~from t =
    Queue.iter (write_without_pushback t) from;
    return ()

  let close t = Pipe.close t; flush t >>= fun _ -> return ()
  let read = Pipe.read
  let iter = Pipe.iter
  let iter_without_pushback = Pipe.iter_without_pushback

  module Writer = struct
    type 'a t = 'a Pipe.Writer.t
  end
  module Reader = struct
    type 'a t = 'a Pipe.Reader.t
  end

end

module Scheduler = struct
  let go = Scheduler.go
  let shutdown n = Shutdown.shutdown n
end

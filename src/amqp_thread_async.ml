(** Async compatability layer *)

open Async.Std

module Deferred = struct
  type 'a t = 'a Deferred.t
  let all_unit = Deferred.all_unit
end

let (>>=) = (>>=)
let return = return

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
end
module Reader = struct
  type t = Reader.t
  let close = Reader.close
  let really_read_bigsubstring = Reader.really_read_bigsubstring
end
module Writer = struct
  type t = Writer.t
  let write_bigstring = Writer.write_bigstring
  let close = Writer.close
  let flushed = Writer.flushed
  let write = Writer.write
end

let spawn = don't_wait_for


(* Pipes *)
module Pipe = struct
  type ('a, 'phantom) t = ('a, 'phantom) Pipe.t
  let create = Pipe.create
  let set_size_budget = Pipe.set_size_budget
  let downstream_flushed t = Pipe.downstream_flushed t
  let interleave_pipe = Pipe.interleave_pipe
  let transfer_in = Pipe.transfer_in
  let close = Pipe.close
  let read = Pipe.read
  module Writer = struct
    type 'a t = 'a Pipe.Writer.t
  end
  module Reader = struct
    type 'a t = 'a Pipe.Reader.t
  end
  let write = Pipe.write
  let write_without_pushback = Pipe.write_without_pushback

end

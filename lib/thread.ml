module type T = sig
  module Deferred : sig
    type 'a t

    val all_unit : unit t list -> unit t
    val try_with : (unit -> 'a t) -> [> `Error of exn | `Ok of 'a ] t
    module List : sig
      val init : f:(int -> 'a t) -> int -> 'a list t
      val iter : ?how:[`Sequential | `Parallel] -> f:('a -> unit t) -> 'a list -> unit t
    end
  end
  val ( >>= ) : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t
  val ( >>| ) : 'a Deferred.t -> ('a -> 'b) -> 'b Deferred.t
  val return : 'a -> 'a Deferred.t
  val after : float -> unit Deferred.t
  val spawn : unit Deferred.t -> unit
  val with_timeout : int -> 'a Deferred.t -> [ `Result of 'a | `Timeout ] Deferred.t

  module Ivar : sig
    type 'a t
    val create : unit -> 'a t
    val create_full : 'a -> 'a t
    val fill : 'a t -> 'a -> unit
    val read : 'a t -> 'a Deferred.t
    val is_full : 'a t -> bool
    val fill_if_empty : 'a t -> 'a -> unit
  end

  module Reader : sig
    type t
    val close : t -> unit Deferred.t
    val read : t -> bytes -> [ `Eof of int | `Ok ] Deferred.t
  end
  module Writer : sig
    type t
    val write : t -> string -> unit
    val close : t -> unit Deferred.t
    val flush : t -> unit Deferred.t
  end

  module Tcp : sig
    val connect : ?nodelay:unit -> string -> int ->
      (Reader.t * Writer.t) Deferred.t
  end

  module Log : sig
    val debug : ('a, unit, string, unit) format4 -> 'a
    val info : ('a, unit, string, unit) format4 -> 'a
    val error : ('a, unit, string, unit) format4 -> 'a
  end
  module Pipe : sig
    module Writer : sig type 'a t end
    module Reader : sig type 'a t end

    val create : unit -> 'a Reader.t * 'a Writer.t
    val set_size_budget : 'a Writer.t -> int -> unit
    val flush : 'a Writer.t -> unit Deferred.t
    val interleave_pipe : 'a Reader.t Reader.t -> 'a Reader.t
    val write : 'a Writer.t -> 'a -> unit Deferred.t
    val write_without_pushback : 'a Writer.t -> 'a -> unit
    val transfer_in : from:'a Queue.t -> 'a Writer.t -> unit Deferred.t
    val close : 'a Writer.t -> unit Deferred.t
    val read : 'a Reader.t -> [ `Eof | `Ok of 'a ] Deferred.t
    val iter : 'a Reader.t -> f:('a -> unit Deferred.t) -> unit Deferred.t
    val iter_without_pushback : 'a Reader.t -> f:('a -> unit) -> unit Deferred.t
    val close_without_pushback : 'a Writer.t -> unit
  end

  module Scheduler : sig
    val go : unit -> unit
    val shutdown : int -> unit
  end
end

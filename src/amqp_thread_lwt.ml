(** Async compatability layer *)


module Deferred = struct
  type 'a t = 'a Lwt.t
  let all_unit = Lwt.join
  module List = struct
    let init ~f n =
      let rec inner = function
        | i when i = n -> []
        | i -> i :: inner (i - 1)
      in
      inner 0 |> Lwt_list.map_p f
    let iter ~f l = Lwt_list.iter_p f l
  end
end

let (>>=) = Lwt.(>>=)
let (>>|) = Lwt.(>|=)
let return = Lwt.return
let after _ = failwith "Not implemented"
let spawn t = Lwt.async (fun () -> t)

module Ivar = struct
  type 'a state = Empty of 'a Lwt_condition.t
                | Full of 'a
  type 'a t = 'a state ref
  let create () = ref (Empty (Lwt_condition.create ()))
  let create_full v = ref (Full v)
  let fill t v =
    match !t with
    | Empty c -> Lwt_condition.broadcast c v;
      t := Full v
    | Full _ -> failwith "Var already filled"

  let read t =
    match !t with
    | Empty c -> Lwt_condition.wait c
    | Full v -> return v

  let is_full t =
    match !t with
    | Empty _ -> false
    | Full _ -> true

  let fill_if_empty t v =
    if (not (is_full t)) then
      fill t v
end

(* Pipes. Bound are not implemented yet .*)
module Pipe = struct
  type 'a elem = Data of 'a
               | Flush of unit Lwt_condition.t

  module Reader = struct
    type 'a t = 'a elem Lwt_stream.t
  end
  module Writer = struct
    type 'a t =  'a elem option -> unit
  end

  let create () =
    let stream, push = Lwt_stream.create () in
    ( stream, push )

  (** Not supported yet *)
  let set_size_budget _t _budget = ()

  (* Can be readers and writers. *)
  let flush (t : 'a Writer.t) =
    let cond = Lwt_condition.create () in
    t (Some (Flush cond));
    Lwt_condition.wait cond

  let rec read (t : 'a Reader.t) =
    Lwt_stream.get t >>= function
    | None -> return `Eof
    | Some Data d -> return (`Ok d)
    | Some Flush cond ->
      Lwt_condition.signal cond ();
      read t

  let write_without_pushback (t : 'a Writer.t) data =
    t (Some (Data data))

  let write (t : 'a Writer.t) (data : 'a) =
    write_without_pushback t data;
    return ()

  (* Pipe of pipes *)
  let interleave_pipe (t: 'a Reader.t Reader.t) : 'a Reader.t =
    let (reader, writer) = create () in
    let rec copy t =
      Lwt_stream.get t >>= function
      | Some n ->
        writer (Some n);
        copy t
      | None -> return ()
    in
    let run = function
      | Data t -> copy t
      | Flush _ -> failwith "Cannot flush this one"
    in
    spawn (Lwt_stream.iter_p run t);
    reader


  let transfer_in (t: 'a Writer.t) ~from:queue : unit Deferred.t =
    (* Just copy all elements *)
    Core_kernel.Std.Queue.iter queue ~f:(write_without_pushback t);
    return ()

  let close (t: 'a Writer.t) =
    t None

  let iter ~f (t: 'a Reader.t) =
    let rec inner () =
      read t >>= function
      | `Eof -> return ()
      | `Ok d -> f d; inner ()
    in
    inner ()

  let iter_without_pushback = iter

end

(** Byte readers *)
module Reader = struct
  type t = Lwt_unix.file_descr
  let close fd = Lwt_unix.close fd
  let really_read_bigsubstring (t : t) (bigsubstr : Core.Std.Bigsubstring.t)
    : [ `Eof of int | `Ok ] Deferred.t =
    let len = Core.Std.Bigsubstring.length bigsubstr in
    (* Can we get a string of equal size, or do we need to create a new? *)
    let buffer = Bytes.create len in
    let rec inner = function
      | offset when offset = len ->
        (* All read copy to bigsubstr *)
        Core.Std.Bigsubstring.blit_from_string bigsubstr ~src:buffer ~src_pos:0 ~len;
        return `Ok
      | offset ->
        Lwt_unix.recv t buffer offset (len - offset) [] >>= fun read ->
        inner (read + offset)
    in
    inner 0
end

(** Byte writers *)
module Writer = struct
  type t = string Pipe.Writer.t

  let write_bigstring t bigstr =
    let data = Core.Std.Bigstring.to_string bigstr in
    Pipe.write_without_pushback t data

  let close t = Pipe.close t; return ()
  let flush t = Pipe.flush t
  let write t data = Pipe.write_without_pushback t data
end

module Tcp = struct

  let write fd data =
    let len = String.length data in
    let rec inner = function
      | offset when offset = len -> return ()
      | offset -> Lwt_unix.send fd data offset (len - offset) [] >>= fun sent ->
        inner (offset + sent)
    in
    inner 0

  let connect host port =
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    let inet_addr = Unix.inet_addr_of_string host in
    Lwt_unix.connect fd (Lwt_unix.ADDR_INET (inet_addr, port)) >>= fun () ->
    (* Start a process that writes *)
    let (reader, writer) = Pipe.create () in
    spawn (Pipe.iter ~f:(write fd) reader);
    return (fd, fd, writer)

  let nodelay (fd: Lwt_unix.file_descr) (value : bool) =
    Lwt_unix.(setsockopt fd TCP_NODELAY value)
end

module Scheduler = struct
  let cond = Lwt_condition.create ()
  let go () = Lwt_main.run (Lwt_condition.wait cond)
  let shutdown (n : int) = Lwt_condition.signal cond n
end

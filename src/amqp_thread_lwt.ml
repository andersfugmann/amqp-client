(** Async compatability layer *)


let log fmt = Printf.eprintf (fmt ^^ "\n%!")
module Deferred = struct
  type 'a t = 'a Lwt.t
  let all_unit = Lwt.join
  module List = struct
    let init ~f n =
      let rec inner = function
        | i when i = n -> []
        | i -> i :: inner (i + 1)
      in
      inner 0 |> Lwt_list.map_p f
    let iter ~f l = Lwt_list.iter_p f l
  end
end

let (>>=) = Lwt.(>>=)
let (>>|) = Lwt.(>|=)
let return = Lwt.return
let after ms = Lwt_unix.sleep (ms /. 1000.0)
let spawn (t : unit Lwt.t) = Lwt.async (fun () -> t)

module Ivar = struct
  type 'a state = Empty of 'a Lwt_condition.t
                | Full of 'a
  type 'a t = { mutable state: 'a state }
  let create () =
    { state = Empty (Lwt_condition.create ()) }

  let create_full v = { state = Full v }

  let fill t v =
    match t.state with
    | Empty c -> Lwt_condition.broadcast c v;
      t.state <- Full v
    | Full _ -> failwith "Var already filled"

  let read t =
    match t.state with
    | Empty c ->
      Lwt_condition.wait c >>= fun v ->
      return v
    | Full v ->
      return v

  let is_full t =
    match t.state with
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

  (* Pipe of pipes. Must spawn more *)
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
      | Flush _ ->
        log "Got flush";
        failwith "Cannot flush this one"
    in
    spawn (Lwt_stream.iter_p run t);
    reader


  let transfer_in (t: 'a Writer.t) ~from:queue : unit Deferred.t =
    (* Just copy all elements *)
    Core_kernel.Std.Queue.iter queue ~f:(write_without_pushback t);
    return ()

  (* Close should include flush *)
  let close (t: 'a Writer.t) =
    let cond = Lwt_condition.create () in
    t (Some (Flush cond));
    t None;
    Lwt_condition.wait cond

  let iter ~f (t: 'a Reader.t) =
    let rec inner () =
      read t >>= function
      | `Eof ->
        log "Iter: eof";
        return ()
      | `Ok d -> f d >>= fun () ->
        inner ()
    in
    inner ()

  let iter_without_pushback ~f t =
    let rec inner () =
      read t >>= function
      | `Eof ->
        log "Iter_without_pushback: eof";
        return ()
      | `Ok d -> f d; inner ()
    in
    inner ()

end

(** Byte readers *)
module Reader = struct
  type t = Lwt_io.input_channel
  let close t =
    log "Try close lwt";
    Lwt_io.close t >>|
    fun () -> log "Close done lwt "

  let really_read_bigsubstring (t : t) (bigsubstr : Core.Std.Bigsubstring.t)
    : [ `Eof of int | `Ok ] Deferred.t =
    let len = Core.Std.Bigsubstring.length bigsubstr in
    (* Can we get a string of equal size, or do we need to create a new? *)
    let buffer = Bytes.create len in
    Lwt.catch
      (fun () ->
         Lwt_io.read_into_exactly t buffer 0 len >>= fun () ->
         Core.Std.Bigsubstring.blit_from_string bigsubstr ~src:buffer ~src_pos:0 ~len;
         return `Ok)
      (fun _exn -> return (`Eof 0))
end

(** Byte writers *)
module Writer = struct
  type t = string Pipe.Writer.t

  let write_bigstring t bigstr =
    let data = Core.Std.Bigstring.to_string bigstr in
    Pipe.write_without_pushback t data

  let close t = Pipe.close t (* TODO: Need to close input  *)
  let flush t = Pipe.flush t
  let write t data = Pipe.write_without_pushback t data
end
(* Just need to convert to lwt_io. *)
module Tcp = struct

  let connect host port =
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.gethostbyname host >>= fun entry ->
    let sock_addr = (Lwt_unix.ADDR_INET (entry.Lwt_unix.h_addr_list.(0), port)) in
    Lwt_io.open_connection ~fd sock_addr >>= fun (ic, oc) ->
    (* Start a process that writes *)
    let (reader, writer) = Pipe.create () in
    spawn (Pipe.iter ~f:(fun str ->
        Lwt_io.write oc str) reader);
    return (fd, ic, writer)

  let nodelay (fd: Lwt_unix.file_descr) (value : bool) =
    Lwt_unix.(setsockopt fd TCP_NODELAY value)
end

module Scheduler = struct
  let cond = Lwt_condition.create ()
  let go () = Lwt_main.run (Lwt_condition.wait cond)
  let shutdown (n : int) = Lwt_condition.signal cond n
end

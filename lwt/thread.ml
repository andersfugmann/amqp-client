open Lib
let (>>=) = Lwt.(>>=)
let (>>|) = Lwt.(>|=)
let return = Lwt.return
let after ms = Lwt_unix.sleep (ms /. 1000.0)
let spawn t = Lwt.ignore_result t

let with_timeout seconds deferred =
  Lwt.pick [
    Lwt_unix.sleep (float_of_int seconds) >>| (fun () -> `Timeout);
    deferred >>| (fun success -> `Result success)
  ]

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

module Deferred = struct
  type 'a t = 'a Lwt.t
  let all_unit = Lwt.join
  let try_with f =
    let open Lwt in
    let var = Ivar.create () in
    let hook = !async_exception_hook in
    async_exception_hook := (Ivar.fill var);
    catch (fun () -> (f () >>= fun r -> return (`Ok r)) <?>
           (Ivar.read var >>= fun e -> return (`Error e)))
      (fun exn -> return (`Error exn)) >>= fun x ->
    async_exception_hook := hook;
    return x

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

module Log = struct
  let section = Lwt_log.Section.make "amqp-client"

  let debug fmt = Lwt_log.ign_debug_f ~section fmt
  let info fmt = Lwt_log.ign_info_f ~section fmt
  let error fmt = Lwt_log.ign_error_f ~section fmt
end

(* Pipes. Bounds are not implemented yet. *)
module Pipe = struct
  type 'a elem = Data of 'a
               | Flush of unit Lwt_condition.t

  type 'a t = { cond: unit Lwt_condition.t;
                queue: 'a elem Ocaml_lib.Queue.t;
                mutable closed: bool;
              }

  module Reader = struct
    type nonrec 'a t = 'a t
  end

  module Writer = struct
    type nonrec 'a t = 'a t
  end

  let create () =
    let t = { cond = Lwt_condition.create ();
              queue = Ocaml_lib.Queue.create ();
              closed = false;
            } in
    (t, t)

  (** Not supported yet *)
  let set_size_budget _t _budget = ()

  (* Can be readers and writers. *)
  let flush t =
    match Ocaml_lib.Queue.is_empty t.queue with
    | true -> return ()
    | false ->
      let cond = Lwt_condition.create () in
      Ocaml_lib.Queue.push (Flush cond) t.queue;
      Lwt_condition.wait cond

  let rec read_raw t =
    match Ocaml_lib.Queue.is_empty t.queue with
    | true ->
      begin match t.closed with
      | true -> return `Eof
      | false ->
        Lwt_condition.wait t.cond >>= fun () ->
        read_raw t
      end
    | false ->
      return (`Ok (Ocaml_lib.Queue.pop t.queue))

  let rec read t =
    read_raw t >>= function
    | `Eof -> return `Eof
    | `Ok (Data d) -> return @@ `Ok d
    | `Ok (Flush cond) ->
      Lwt_condition.signal cond ();
      read t

  let write_raw t data =
    Ocaml_lib.Queue.push data t.queue;
    Lwt_condition.broadcast t.cond ()

  let write_without_pushback t data =
    write_raw t (Data data)

  let write t data =
    write_without_pushback t data;
    return ()

  let rec iter t ~f =
    read t >>= function
    | `Eof -> return ()
    | `Ok d -> f d >>= fun () -> iter t ~f

  let rec iter_without_pushback t ~f =
    read t >>= function
    | `Eof -> return ()
    | `Ok d -> f d; iter_without_pushback t ~f

  (* Pipe of pipes. Must spawn more *)
  let interleave_pipe t =
    let (reader, writer) = create () in
    let rec copy t =
      read_raw t >>= function
      | `Eof -> return ()
      | `Ok data ->
        write_raw writer data;
        copy t
    in
    spawn (iter_without_pushback t ~f:(fun p -> spawn (copy p)));
    reader

  let transfer_in ~from:queue t =
    Ocaml_lib.Queue.iter (write_without_pushback t) queue;
    return ()

  let close t =
    t.closed <- true;
    begin match Ocaml_lib.Queue.is_empty t.queue with
    | true -> return ()
    | false -> flush t
    end >>= fun () ->
    return ()

  let close_without_pushback t =
    t.closed <- true;
    Lwt_condition.broadcast t.cond ()

end

module Reader = struct
  type t = Lwt_io.input_channel
  let close t = Lwt_io.close t

  let read input buf : [ `Eof of int | `Ok ] Deferred.t =
    let len = Bytes.length buf in
    let rec inner = function
      | n when n = len ->
          return `Ok
      | n -> begin
          Lwt.catch
            (fun () -> Lwt_io.read_into input buf n (len - n))
            (fun _exn -> return 0) >>= function
          | 0 -> return (`Eof n)
          | read -> inner (n + read)
        end
    in
    inner 0
end

module Writer = struct
  type t = string Pipe.Writer.t
  let close t = Pipe.close t
  let flush t = Pipe.flush t
  let write t data = Pipe.write_without_pushback t data
end

module Tcp = struct

  let connect ?nodelay host port =
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.gethostbyname host >>= fun entry ->
    let sock_addr = (Lwt_unix.ADDR_INET (entry.Lwt_unix.h_addr_list.(0), port)) in
    Lwt_io.open_connection ~fd sock_addr >>= fun (ic, oc) ->
    (* Start a process that writes *)
    let (reader, writer) = Pipe.create () in
    spawn (Pipe.iter ~f:(fun str ->
        Lwt_io.write oc str) reader);

    (match nodelay with
     | Some () -> Lwt_unix.(setsockopt fd TCP_NODELAY true)
     | None -> ());
    return (ic, writer)

end

module Scheduler = struct
  let cond = Lwt_condition.create ()
  let go () = Lwt_main.run (Lwt_condition.wait cond) |> ignore
  let shutdown (n : int) = Lwt_condition.signal cond n
end

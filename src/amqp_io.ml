(** Internal *)
open Core.Std

let log fmt =
  (* Printf.eprintf (fmt ^^ "\n%!") *)
  Printf.ifprintf stderr fmt

module Input = struct
  open Bigstring
  type t = { buf: Bigstring.t; mutable offset: int }
  let init ?(offset=0) buf = { buf; offset }
  let create len = { buf = Bigstring.create len; offset=0 }
  let read f n t =
    let r = f t.buf ~pos:t.offset in t.offset <- t.offset + n; r
  let string t n =
    let s = to_string ~pos:t.offset ~len:n t.buf in
    t.offset <- t.offset + n;
    s
  let octet = read unsafe_get_uint8 1
  let short = read unsafe_get_uint16_be 2
  let long  = read unsafe_get_uint32_be 4
  let longlong = read unsafe_get_int64_be_exn 8
  let float t = read unsafe_get_int32_t_be 4 t |> Int32.to_float
  let double t = read unsafe_get_int64_t_be 8 t |> Int64.to_float
  let length t = length t.buf - t.offset
  let has_data t = length t > 0
  let offset t = t.offset
  let copy t ~dst_pos ~len dst =
    Bigstring.To_string.blit ~src:t.buf ~src_pos:t.offset ~dst ~dst_pos ~len;
    t.offset <- t.offset + len
  let destroy t = unsafe_destroy t.buf
end

module Output = struct
  open Bigstring
  type t = { mutable buf: Bigstring.t; mutable offset: int }
  let create len = { buf = Bigstring.create len; offset = 0}
  let write f n t v =
    f t.buf ~pos:t.offset v; t.offset <- t.offset + n
  let get t = t.buf
  let string t ?(src_pos=0) ?len src =
    let len = match len with
      | Some l -> l
      | None -> String.length src
    in
    Bigstring.From_string.blit ~src ~src_pos ~dst:t.buf ~dst_pos:t.offset ~len;
    t.offset <- t.offset + len

  let octet = write unsafe_set_uint8 1

  let short = write unsafe_set_uint16_be 2
  let short_ref t =
    let offset = t.offset in
    t.offset <- t.offset + 2;
    fun v -> unsafe_set_uint16_be t.buf ~pos:offset v

  let long = write unsafe_set_uint32_be 4
  let longlong = write unsafe_set_int64_be 8
  let float = write (fun t ~pos v -> unsafe_set_int32_t_be ~pos t (Int32.of_float v)) 4
  let double = write (fun t ~pos v -> unsafe_set_int64_t_be ~pos t (Int64.of_float v)) 8
  let size_ref t =
    let offset = t.offset in
    t.offset <- offset + 4;
    fun extra ->
      unsafe_set_uint32_be t.buf ~pos:offset (t.offset - (offset + 4) + extra)
end

module Sizer = struct
  type t = int ref
  let (+=) a b = a := !a + b
  let init () = ref 0
  let string t s =
    t += (String.length s)
  let octet t _ = t += 1
  let short t _ = t += 2
  let short_ref t =
    t += 2;
    fun _ -> ()
  let long t _ = t += 4
  let longlong t _ = t += 8
  let float t _ = t += 4
  let double t _ = t += 8
  let size_ref t _ =
    t += 4;
    fun _ -> ()
end

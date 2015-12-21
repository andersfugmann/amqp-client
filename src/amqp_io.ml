(** Internal *)
open Core.Std

let log fmt =
  Printf.ifprintf stderr (fmt ^^ "\n%!")

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
  let _empty = Bigstring.create 0
  type t = { mutable buf: Bigstring.t; mutable offset: int; apply: bool }
  let create len = { buf = Bigstring.create len; offset = 0; apply = true }
  let sizer () = { buf = _empty; offset = 0; apply = false }
  let write f n t v =
    if t.apply then
      f t.buf ~pos:t.offset v;
    t.offset <- t.offset + n
  let get t = t.buf
  let string t ?(src_pos=0) ?len src =
    let len = match len with
      | Some l -> l
      | None -> String.length src
    in
    if (t.apply) then
        Bigstring.From_string.blit ~src ~src_pos ~dst:t.buf ~dst_pos:t.offset ~len;
    t.offset <- t.offset + len

  let octet = write unsafe_set_uint8 1
  let short = write unsafe_set_uint16_be 2
  let short_ref t =
    let offset = t.offset in
    t.offset <- t.offset + 2;
    fun v ->
      if (t.apply) then unsafe_set_uint16_be t.buf ~pos:offset v

  let long = write unsafe_set_uint32_be 4
  let longlong = write unsafe_set_int64_be 8
  let float = write (fun t ~pos v -> unsafe_set_int32_t_be ~pos t (Int32.of_float v)) 4
  let double = write (fun t ~pos v -> unsafe_set_int64_t_be ~pos t (Int64.of_float v)) 8
  let size_ref t =
    let offset = t.offset in
    t.offset <- offset + 4;
    fun () ->
      if t.apply then unsafe_set_uint32_be t.buf ~pos:offset (t.offset - (offset + 4))

  let size t = t.offset
end

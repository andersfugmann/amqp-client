(** Internal *)

module Input = struct
  open EndianString.BigEndian
  type t = { buf: String.t; mutable offset: int }
  let init ?(offset=0) buf = { buf; offset }
  let read f n t =
    let r = f t.buf t.offset in
    t.offset <- t.offset + n;
    r
  let string t len =
    let s = String.sub t.buf t.offset len in
    t.offset <- t.offset + len;
    s
  let octet = read get_uint8 1
  let short = read get_uint16 2
  let long t = read get_int32 4 t |> Int32.to_int
  let longlong t = read get_int64 8 t |> Int64.to_int
  let float = read get_float 4
  let double = read get_double 8

  let length t = String.length t.buf - t.offset
  let has_data t = length t > 0
  let offset t = t.offset

  let copy t ~dst_pos ~len (dst:Bytes.t) =
    Bytes.blit_string t.buf t.offset dst dst_pos len;
    t.offset <- t.offset + len
end

module Output = struct
  open EndianString.BigEndian
  type t = { mutable buf: Bytes.t; mutable offset: int; apply: bool }
  let create len = { buf = Bytes.create len; offset = 0; apply = true }

  (* The sizer dont actually do anything, but record space needed *)
  let sizer () = { buf = Bytes.create 0; offset = 0; apply = false }
  let write f n t v =
    if t.apply then
      f t.buf t.offset v;
    t.offset <- t.offset + n
  let get t : Bytes.t = t.buf
  let string t ?(src_pos=0) ?len src =
    let len = match len with
      | Some l -> l
      | None -> String.length src
    in
    if (t.apply) then
      Bytes.blit_string src src_pos t.buf t.offset len;
    t.offset <- t.offset + len

  let octet = write set_int8 1
  let short = write set_int16 2
  let short_ref t =
    let offset = t.offset in
    t.offset <- t.offset + 2;
    fun v ->
      if (t.apply) then set_int16 t.buf offset v

  let long = write (fun t pos v -> set_int32 t pos (Int32.of_int v)) 4
  let longlong = write (fun t pos v -> set_int64 t pos (Int64.of_int v)) 8
  let float = write set_float 4
  let double = write set_double 8
  let size_ref t =
    let offset = t.offset in
    t.offset <- offset + 4;
    fun () ->
      if t.apply then set_int32 t.buf offset (Int32.of_int (t.offset - (offset + 4)))

  let size t = t.offset
end

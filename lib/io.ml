(** Internal *)

module Input = struct
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

  let octet = read String.get_uint8 1
  let short = read String.get_uint16_be 2
  let long t = read String.get_int32_be 4 t |> Int32.to_int
  let longlong t = read String.get_int64_be 8 t |> Int64.to_int
  let float = read (fun s l -> String.get_int32_be s l |> Int32.float_of_bits) 2
  let double = read (fun s l -> String.get_int64_be s l |> Int64.float_of_bits) 8

  let length t = String.length t.buf - t.offset
  let has_data t = length t > 0
  let offset t = t.offset

  let copy t ~dst_pos ~len (dst:Bytes.t) =
    Bytes.blit_string t.buf t.offset dst dst_pos len;
    t.offset <- t.offset + len
end

module Output = struct
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

  let octet = write Bytes.set_int8 1
  let short = write Bytes.set_int16_be 2
  let short_ref t =
    let offset = t.offset in
    t.offset <- t.offset + 2;
    fun v ->
      if (t.apply) then Bytes.set_int16_be t.buf offset v

  let long = write (fun t pos v -> Bytes.set_int32_be t pos (Int32.of_int v)) 4
  let longlong = write (fun t pos v -> Bytes.set_int64_be t pos (Int64.of_int v)) 8
  let float = write (fun t pos v -> Bytes.set_int32_be t pos (Int32.bits_of_float v)) 4
  let double = write (fun t pos v -> Bytes.set_int64_be t pos (Int64.bits_of_float v)) 8
  let size_ref t =
    let offset = t.offset in
    t.offset <- offset + 4;
    fun () ->
      if t.apply then Bytes.set_int32_be t.buf offset (Int32.of_int (t.offset - (offset + 4)))

  let size t = t.offset
end

(* Simple reader and writer based on local buffers *)

module Input = struct
  open EndianString.BigEndian
  type t = { buf: string; mutable offset: int }
  let create ?(offset=0) buf = { buf; offset }
  let read  f n t = let r = f t.buf t.offset in t.offset <- t.offset + n; r
  let string t n =
    let r = String.sub t.buf t.offset n in
    t.offset <- t.offset + n;
    r
  let octet = read get_uint8 1
  let short = read get_int16 2
  let long t = read get_int32 4 t |> Int32.to_int
  let longlong t = read get_int64 8 t |> Int64.to_int
  let float = read get_float 4
  let double = read get_double 8
end

module Output = struct
  open EndianBytes.BigEndian
  type t = { buf: Bytes.t; mutable offset: int }
  let create ?(offset=0) buf = { buf; offset }
  let write f n t v = f t.buf t.offset v; t.offset <- t.offset + n

  let string t s =
    let n = String.length s in
    Bytes.blit_string s 0 t.buf t.offset n;
    t.offset <- t.offset + n

  let octet = write set_int8 1
  let short = write set_int16 2
  let long t v = write set_int32 4 t (Int32.of_int v)
  let longlong t v = write set_int64 8 t (Int64.of_int v)
  let float = write set_float 4
  let double = write set_double 8
end

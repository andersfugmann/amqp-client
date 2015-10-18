(* Simple reader and writer based on local buffers *)
let log fmt = Async.Std.printf (fmt ^^ "\n%!")

module Input = struct
  open EndianString.BigEndian
  type t = { buf: string; mutable offset: int }
  let create ?(offset=0) buf = { buf; offset }
  let read  f n t = let r = f t.buf t.offset in t.offset <- t.offset + n; r
  let string t n =
    log "string {buf:%d; offset:%d} %d\n%!" (String.length t.buf) t.offset n;
    let r = String.sub t.buf t.offset n in
    t.offset <- t.offset + n;
    r
  let octet = read get_uint8 1
  let short = read get_int16 2
  let long t = read get_int32 4 t |> Int32.to_int
  let longlong t = read get_int64 8 t |> Int64.to_int
  let float = read get_float 4
  let double = read get_double 8

  let length t = String.length t.buf - t.offset
  let has_data t = log "length: %d" (length t);
    length t > 0
end

module Output = struct
  open EndianBytes.BigEndian
  type t = { mutable buf: Bytes.t; mutable offset: int; start: int }
  let init ?(offset=0) ?(start=0) buf = { buf; offset; start }
  let create ?(size=256) () = { buf = Bytes.create size; offset = 0; start = 0 }
  let size_left {buf; offset; start} = Bytes.length buf - offset - start
  let grow t =
    t.buf <- Bytes.extend t.buf 0 (Bytes.length t.buf)
  let write f n t v =
    if size_left t < n then grow t;
    f t.buf t.offset v; t.offset <- t.offset + n
  let buffer t = t.buf
  let length t = t.offset - t.start

  let string t s =
    let len =String.length s in
    begin match Bytes.length t.buf - t.offset with
      | n when n < len ->
        t.buf <- Bytes.extend t.buf 0 (len - n)
      | _ -> ()
    end;
    Bytes.blit_string s 0 t.buf t.offset len;
    t.offset <- t.offset + len

  let octet = write set_int8 1

  let short = write set_int16 2
  let short_ref t =
    let offset = t.offset in
    short t 0;
    (* evaluate t.buf late, as it might change before calling the function *)
    fun v -> set_int16 t.buf offset v

  let long t v = write set_int32 4 t (Int32.of_int v)
  let longlong t v = write set_int64 8 t (Int64.of_int v)
  let float = write set_float 4
  let double = write set_double 8
  let size_ref t =
    let offset = t.offset in
    long t 0;
    fun extra ->
      set_int32 t.buf offset (Int32.of_int (t.offset - (offset + 4) + extra))

  let sub t ~start ~length =
    let offset = min (start+length) (Bytes.length t.buf) in
    { start; offset; buf = t.buf }

end

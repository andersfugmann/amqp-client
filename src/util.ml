open Batteries
open Types

let request0 _ =
  fun _ -> failwith "Not implemented"

let reply0 _ =
  fun _ -> failwith "Not implemented"

let reply1 (cls, mth, spec, make, _apply) (r_class, r_method, r_spec, _r_make, r_apply) =
  let read = read spec in
  let write = write r_spec in
  fun channel callback ->
    let callback data =
      let req = read make (IO.input_string data) in
      let rep = callback req in
      let out = r_apply (write (IO.output_string ())) rep in
      Channel.send channel r_class r_method (IO.close_out out)
    in
    Channel.receive channel cls mth callback

let request1 (cls, mth, spec, _make, apply) (r_cls, r_mth, r_spec, r_make, _r_apply) : (Channel.t -> 'a -> ('b -> unit)) =
  let write = write spec in
  let read = read r_spec in
  fun channel msg callback ->
    let req =
      apply (write (IO.output_string ())) msg
      |> IO.close_out
    in
    Channel.send channel cls mth req;
    let callback data =
      let rep = read r_make (IO.input_string data) in
      callback rep
    in
    Channel.receive channel r_cls r_mth callback

let request2 _ _ _ =
  fun _ -> failwith "Not implemented"

let reply2 _ _ _ =
  fun _ -> failwith "Not implemented"

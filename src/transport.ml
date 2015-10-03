open Batteries

let protocol_header = "AMQP\x00\x00\x09\x01"

type t = { input: IO.input; output: unit IO.output; }

let connect ~port ~host =
  let open Unix in
  let addr = inet_addr_of_string host in
  let input, output =
    open_connection ~autoclose:false (ADDR_INET (addr, port))
  in

  IO.really_output output protocol_header 0 (String.length protocol_header) |> ignore;
  IO.flush output;
  { input; output; }

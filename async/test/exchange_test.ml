open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  Exchange.declare channel ~auto_delete:true Exchange.direct_t (uniq "test1") >>= fun exchange1 ->
  Log.info "Exchange declared";
  Exchange.declare channel ~auto_delete:true Exchange.direct_t (uniq "test2") >>= fun exchange2 ->
  Log.info "Exchange declared";
  Exchange.bind channel ~source:exchange1 ~destination:exchange2 (`Queue (uniq "test")) >>= fun () ->
  Log.info "Exchange Bind";
  Exchange.unbind channel ~source:exchange1 ~destination:exchange2 (`Queue (uniq "test")) >>= fun () ->
  Log.info "Exchange Unbind";
  Exchange.delete channel exchange1 >>= fun () ->
  Log.info "Exchange deleted";
  Exchange.delete channel exchange2 >>= fun () ->
  Log.info "Exchange deleted";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

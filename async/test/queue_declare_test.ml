open Amqp
open Thread

let uniq s =
  Printf.sprintf "%s_%d_%s" (Filename.basename Sys.argv.(0)) (Unix.getpid ()) s

let handler var { Message.message = (_, body); _ } = Ivar.fill var body; return ()

let declare ~channel name =
  let queue_name = uniq name in
  Queue.declare channel ~auto_delete:true queue_name >>= fun queue ->
  Log.info "Created queue: %s === %s" (Queue.name queue) queue_name;
  match Queue.name queue = queue_name with
  | false -> failwith (Printf.sprintf "Queue name mismatch: %s != %s" (Queue.name queue) queue_name)
  | true -> return queue

let check_declare_autogenerate ~channel =
  Queue.declare channel ~auto_delete:true ~autogenerate:true "" >>= fun queue ->
  Log.info "Created queue: %s" (Queue.name queue);
  let _ = try
      Queue.declare channel ~auto_delete:true
        ~autogenerate:true "non-empty-name"
    with Invalid_argument msg ->
      assert (msg = "Queue.declare name must be empty if autogenerate is true.");
      return queue
  in
  let _ = try
      Queue.declare channel ~auto_delete:true ""
    with Invalid_argument msg ->
      assert (msg = "Queue.declare autogenerate must be true if name is empty.");
      return queue
  in
  return queue


let test =
  let port = Sys.getenv_opt "AMQP_PORT" |> function Some port -> Some (int_of_string port) | None -> None in
  Connection.connect ~id:(uniq "") ?port "localhost" >>= fun connection ->
  Log.info "Connection started";
  Connection.open_channel ~id:(uniq "queue.test") Channel.no_confirm connection >>= fun channel ->
  Log.info "Channel opened";
  let queues =
    [0;1;2;3;4;5;6;7;8;9]
    |> List.map (fun i -> Printf.sprintf "queue.test_%d" i)
    |> List.map (declare ~channel)
  in
  let queues = check_declare_autogenerate ~channel :: queues in
  List.fold_left (fun acc queue -> acc >>= fun acc -> queue >>= fun queue -> return (queue :: acc)) (return []) queues >>= fun queues ->
  Log.info "Queues declared";
  List.fold_left (fun acc queue -> acc >>= fun () -> Queue.delete channel queue) (return ()) queues >>= fun () ->
  Log.info "Queues deleted";
  Channel.close channel >>= fun () ->
  Log.info "Channel closed";
  Connection.close connection >>| fun () ->
  Log.info "Connection closed";
  Scheduler.shutdown 0

let _ =
  Scheduler.go ()
let () = Printf.printf "Done\n"

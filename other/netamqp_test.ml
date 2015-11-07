(* This is the sender for receiver_t. Please read the comments there first!

   Also, when trying this example, make sure the receiver is started first
   because the receiver declares the queue.
 *)

open Netamqp_types
open Printf
(*
let () =
  Netamqp_endpoint.Debug.enable := true;
  Netamqp_transport.Debug.enable := true
*)

let esys = Unixqueue.create_unix_event_system()
let p = `TCP(`Inet("localhost", Netamqp_endpoint.default_port))
let ep = Netamqp_endpoint.create p (`AMQP_0_9 `One) esys
let c = Netamqp_connection.create ep
let auth = Netamqp_connection.plain_auth "guest" "guest"

let qname = "test_xy"


let sender c =
  let ch = Netamqp_channel.open_next_s c in
  let channel = Netamqp_channel.number ch in
  eprintf "*** Channel could be opened!\n%!";

  let header =
    `AMQP_0_9
      (`P_basic
         ( None,
           None,
           None,
           Some 1,   (* non-persistent *)
           None,   (* priority *)
           None,
           None,
           None,
           None,
           None,
           None,
           None,
           None,
           None
         )
      ) in

  for n = 100000 downto 0 do
    (* d is the queued message. Note that the body is actually a list of
       mstring (see t_receiver.ml for explanations).
    *)
    let d =
      (header,
       [Netamqp_rtypes.mk_mstring (sprintf "%d" n)]
      ) in

    Netamqp_endpoint.async_c2s
      ep
      (`AMQP_0_9(`Basic_publish(0, "", qname,
                                false, false)))
      (Some d)
      channel;

    (* eprintf "*** Message published!\n%!"; *)
  done;
  eprintf "Send done\n%!"

let receiver c =
  (* Now open the data channel. Channels are multiplexed over connections *)
  let ch = Netamqp_channel.open_next_s c in
  let channel = Netamqp_channel.number ch in
  eprintf "*** Channel could be opened!\n%!";

  eprintf "*** Queue declared!\n%!";

  Netamqp_endpoint.register_async_s2c
    ep
    (`AMQP_0_9 `Basic_deliver)
    channel
    (fun _ -> function
       | Some(_header,body) ->
         begin
           match int_of_string (Netxdr_mstring.concat_mstrings body) with
           | 0 -> (* Shut it down *)
             exit 0
           | n when n mod 1000 = 0 ->
             eprintf "%d\n%!" n
           | _ -> ()
         end
       | None -> failwith "No data"
    );

  let _ = Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Basic_consume(0, qname, "", false, false, false,
                                 false, [] )))
      None
    channel
    (-1.0)
  in
  ()

let _main =
  Netamqp_connection.open_s c [ auth ] (`Pref "en_US") "/";
  eprintf "*** Connection could be opened, and the proto handshake is done!\n%!";

  let ch = Netamqp_channel.open_next_s c in
  let channel = Netamqp_channel.number ch in

  let _ =
    Netamqp_endpoint.sync_c2s_s
      ep
      (`AMQP_0_9 (`Queue_declare(0, qname, false, false, false,
                                 (* auto-delete: *) true, false,
                                 [])))
      None    (* This value would allow to send content data along with the
                   method. Only certain methods permit this, though.
              *)
      channel
      (-1.0)  (* timeout *)
  in

  receiver c;
  sender c;


  Unixqueue.run esys;


  Netamqp_channel.close_s ch;
  eprintf "*** Channel could be closed!\n%!";

  Netamqp_connection.close_s c;
  eprintf "*** Connection could be closed!\n%!"

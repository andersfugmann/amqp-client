
(* Start listening for requests on a queue, and produce a reply to the 'reply-to' *)
let client _queue _handler = ()

(* We need a reply queue. And I only want my own replies. *)
(* So an alternative is to start consuming from the queue, and then use correlation id for replies *)

(* Then we need only one queue (per channel) *)

(* There is really no need for more than one channel *)
let request queue = ()

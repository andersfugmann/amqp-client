(** Main module.
    This module contains convenient module aliases
*)
module Make(Thread : Amqp_thread.T) = struct
  module Message = Amqp_message.Make(Thread)
  module Connection = Amqp_connection.Make(Thread)
  module Queue = Amqp_queue.Make(Thread)
  module Channel = Amqp_channel.Make(Thread)
  module Exchange = Amqp_exchange.Make(Thread)
  module Rpc = Amqp_rpc.Make(Thread)
  module Thread = Thread
end

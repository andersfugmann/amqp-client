include Amqp_client_lwt

let () =
  Lwt_log.default :=
    Lwt_log.channel
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ();
  Lwt_log_core.append_rule "*" Lwt_log_core.Debug;

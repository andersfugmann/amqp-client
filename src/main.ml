let _ =
  let connection = Connection.connect ~host:"127.0.0.1" () in
  Connection.start connection

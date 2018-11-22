include Amqp_client_lib.Thread.T
  with type 'a Deferred.t = 'a Async_kernel.Deferred.t
  and type Writer.t = Async.Writer.t
  and type Reader.t = Async.Reader.t
  and type 'a Pipe.Reader.t = 'a Async.Pipe.Reader.t
  and type 'a Pipe.Writer.t = 'a Async.Pipe.Writer.t
  and type 'a Ivar.t = 'a Async_kernel.Ivar.t

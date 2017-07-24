OCaml client library for AMQP
=============================

Amqp-client is a AMQP client library written in pure OCaml. The
library implements AMQP protocol version 0.9.1 as well as RabbitMQ-specific
extensions. It supports both Core Async and Lwt threading models.

Amqp-client is tested extensively against RabbitMQ, but should work
with any AMQP server.

The library exposes low level protocol handling though ```Amqp_spec```
and ```Amqp_framing``` modules as well as a high level interface
though module ```Amqp```.

The high level interface exposes usage patterns such as
 * create queue
 * consume from a queue
 * post message to a queue
 * create exchange
 * bind a queue to an exchange
 * post message to an exchange
 * RPC client
 * RPC server

The library requires all resources to be explicitly allocated to avoid
crashes because a service is replying on other services to allocate AMQP resources
(exchanges, queues etc.).

Channels and consumers are tagged with an id, host name, pid etc. to ease tracing on AMQP level.

[Documentation for the API (async version)](http://andersfugmann.github.io/amqp-client/).


### Build instructions

The system is not functorized over an abstraction to the threading
model. Instead the build system chooses which threading model
abstraction to be used and stacially compiles it in.  This has the
advantage that files do not need to carry functor boilerplate and that
the compiler can inline function calls.

The disadvantage is that it does not allow users to supply their own
threading model implementation.

To build the library

```make build```

```make install``` will install bLwt and Async depending on availability of Async / Lwt.

### Using the library

To compile using Async do:

```ocamlfind ocamlopt -thread -package amqp-client.async myprog.ml```

To compile using the Lwt version of the library do:

```ocamlfind ocamlopt -thread -package amqp-client.lwt myprog.ml```


### Example

```ocaml
open Async

let host = "localhost"

let run () =
  Amqp.Connection.connect ~id:"MyConnection" host >>= fun connection ->
  Amqp.Connection.open_channel ~id:"MyChannel" Amqp.Channel.no_confirm connection >>= fun channel ->
  Amqp.Queue.declare channel "MyQueue" >>= fun queue ->
  Amqp.Queue.publish channel queue (Amqp.Message.make "My Message Payload") >>= function `Ok ->
  Amqp.Channel.close channel >>= fun () ->
  Amqp.Connection.close connection >>= fun () ->
  Shutdown.shutdown 0; return ()

let _ =
  don't_wait_for (run ());
  Scheduler.go ()
```

Compile with:

```
$ ocamlfind ocamlopt -thread -package amqp-client.async amqp_example.ml -linkpkg -o amqp_example
```

More examples are available here: https://github.com/andersfugmann/amqp-client/tree/master/tests

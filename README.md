OCaml client library for AMQP
=============================
[![BuildStatus](https://travis-ci.org/andersfugmann/amqp-client.svg?branch=master)](https://travis-ci.org/andersfugmann/amqp-client)

Amqp-client is a AMQP client library written in pure OCaml. The
library implements AMQP protocol version 0.9.1 as well as RabbitMQ-specific
extensions. It supports both Core Async and Lwt threading models.

Amqp-client is tested extensively against RabbitMQ, but should work
with any AMQP server.

The library exposes low level protocol handling through ```Amqp_spec```
and ```Amqp_framing``` modules as well as a high level interface
though module ```Amqp```.

The high level interface exposes usage patterns such as
 * create queue
 * consume from a queue
 * post message to a queue
 * create exchange
 * bind a queue to an exchange
 * post message to an exchange
 * RPC client / server

The library requires all resources to be explicitly allocated to avoid
crashes because a service is relying on other services to allocate
AMQP resources (exchanges, queues etc.).

Channels and consumers are tagged with an id, host name, pid etc. to
ease tracing on AMQP level.

The design philiosiphy of the library is *fail fast*, meaning that if
any external state changes (e.g. connection closes unexpectibly, queu
consumption is cancelled) an exception is raised, and It is adviced to
let the process crash and restart initialization rather than going
through the complex task of reparing the state.

[Documentation for the API](http://andersfugmann.github.io/amqp-client/index.html).

### Build infrastructure

The system is not functorized over an abstraction to the threading
model. Instead the build system chooses which threading model
abstraction to be used and stacially compiles it in.  This has the
advantage that files do not need to carry functor boilerplate

The disadvantage is that it does not allow users to supply their own
threading model implementation.

#### Opam
It is recommended to install the package though opam.
You should choose the package matching the concurrency library that your application will use

For Janestreet async: `opam install amqp-client-async`

For Ocsigen Lwt: `opam install amqp-client-lwt`

#### Manual build

To build the library

```make build```

```make install``` will install both Lwt and Async versions.

### Using the library

To compile using Async do:

```ocamlfind ocamlopt -thread -package amqp-client-async myprog.ml```

To compile using the Lwt version of the library do:

```ocamlfind ocamlopt -thread -package amqp-client-lwt myprog.ml```

### Examples

#### Async
Install the async version of the library: `opam install amqp-client-async`

```ocaml
open Async
open Amqp_client_async

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
  Thread_safe.block_on_async_exn run
```

Compile with:
```
$ ocamlfind ocamlopt -thread -package amqp-client-async amqp_example.ml -linkpkg -o amqp_example
```

#### Lwt
Install the lwt version of the library: `opam install amqp-client-lwt`

```ocaml
open Lwt.Infix
open Amqp_client_lwt

let host = "localhost"

let run () =
  Amqp.Connection.connect ~id:"MyConnection" host >>= fun connection ->
  Amqp.Connection.open_channel ~id:"MyChannel" Amqp.Channel.no_confirm connection >>= fun channel ->
  Amqp.Queue.declare channel "MyQueue" >>= fun queue ->
  Amqp.Queue.publish channel queue (Amqp.Message.make "My Message Payload") >>= function `Ok ->
  Amqp.Channel.close channel >>= fun () ->
  Amqp.Connection.close connection >>= fun () ->
  Lwt.return ()

let _ =
  Lwt_main.run (run ())
```

Compile with:

```
$ ocamlfind ocamlopt -thread -package amqp-client-lwt amqp_example.ml -linkpkg -o amqp_example
```

More examples are available here: https://github.com/andersfugmann/amqp-client/tree/master/examples

To compile the examples do: `make examples`, which will place the
binaries under `_build/default/examples/`.

It is recommended to use *dune* for building projects and not invoke
ocaml/ocamlfind from the command line explicitly.

A simple dune file for a project with one file called: `example.ml` looks like this:

```lisp
(executable
  (name example)
  (libraies amqp-client-async)
)
```
To build do `dune build example.exe`. For more information on dune,
see https://dune.readthedocs.io/en/latest/

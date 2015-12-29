OCaml client library for AMQP
=============================

Amqp-client is a AMQP client library written in pure OCaml. The
library implements AMQP protocol version 0.9.1 as well as RabbitMQ specific
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
 * Rpc-client
 * Rpc-server

The library requires all resources to be explicitly allocated to avoid
crashes because a service is replying on other services to allocate AMQP resources
(exchanges, queues etc.).
Channels and consumers are tagged with an id, host name, pid etc. to ease tracing on AMQP level.

Documentation for the API can be found here:
http://andersfugmann.github.io/amqp-client/ (async version)

### Build instructions
The system is not functorized over an abstraction to the threading model. Instead the
build system chooses which threading model abstraction to be used and stacially compiles it in.
This has the advantage that files do not need to carry functor boilerplate and that the compiler can inline function calls.
The disadvantage is that it does not allow users to supply their own threading model implementation.

To build the library using Async:

```make thread=async```

To build the Lwt version the the library:

```make thread=lwt```

```make install``` will install both Lwt and Async depending on availability of Async / Lwt through ocamlfind:


### Using the library
To compile using Async do:

```ocamlfind ocamlopt -package amqp_client.async myprog.ml```

To compile using the Lwt version of the library do:

```ocamlfind ocamlopt -package amqp_client.lwt myprog.ml```


### Examples
Look in the ```tests/``` folder.

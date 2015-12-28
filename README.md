OCaml client library for amqp
=============================

Amqp-client is a AMQP (AMQP 0-9-1) client library written in pure
ocaml. The library implements AMQP spec 0-9-1 as well as as rabbitmq
specific extensions. It supports both Core async and Lwt threading
models.

Amqp-client is tested extensivly against rabbitmq, but should work
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

The goal is to avoid clients crashing because they rely on other
service to create resources (queues / exchanges) on the amqp server,
and in all make use of AMQP simple.

Read the API Documentation here: http://andersfugmann.github.io/amqp-client/

### Build instructions
The system is not functorized over an abstraction to the threading model. Instead the
build system chooses which threding model abstraction to be used and stacially compiles it in.
This has the advantage that files do not need to carry functor boilerplace and that the compiler can inline function calls.
The disadvantage is that it does not allow users to supply their own threading model implementation.

To build the library using
* async:```make thread=async```
* lwt: ```make thread=lwt```

```make install``` will install both lwt and async depending on availability of async / lwt though ocamlfind:


### Using the library
To compile using async do:

```ocamlfind ocamlopt -package amqp_client.async myprog.ml```

For lwt use:
```ocamlfind ocamlopt -package amqp_client.lwt myprog.ml```


### Examples
Look in the ```tests/``` folder.

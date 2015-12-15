OCaml client library for RabbitMQ (AMQP 0-9-1)
=============================

This is a RabbitMQ (AMQP 0-9-1 plus RabbitMQ extensions) client
that uses core async.

The library can make use of rabbitmq specific features
but the goal is to make use of these features optional.

The library exposes low level protocol handling though
Amqp_spec and Amqp_framing module.

And high level interface though module Amqp.
The high level interface exposes usage patterns such as
 * create queue
 * consume from a queue
 * post message to a queue
 * create exchange
 * bind a queue to an exchange
 * post message to an exchange
 * Rpc-client
 * Rpc-server

The idea is to avoid clients crashing because they rely on other service to create resources (queues / exchanges) on the amqp server, and in all make use of AMQP simple.

Read the API Documentation here: http://andersfugmann.github.io/amqp-client/

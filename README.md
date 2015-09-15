ocaml client library for rabbitmq
=================================

The idea is to create a pure and simple implementation for ocaml based
on XML specification of rabbitmq version 0.9.1 extended.

Framework for for cooperative multitasking is not yet determined, and
the idea is to create a pluggable system where network IO is
abstracted to an underlying module that can be implemented with Async,
Algrbraic effects, lwt and more.

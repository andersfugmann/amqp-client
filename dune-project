(lang dune 3.18)
(name amqp-client)

(generate_opam_files true)

(authors "Anders Fugmann")
(maintainers "Anders Fugmann <anders@fugmann.net>")
(license "BSD-3-Clause")
(homepage "https://github.com/andersfugmann/amqp-client")
(bug_reports "https://github.com/andersfugmann/amqp-client/issues")
(source (github andersfugmann/amqp-client))
(documentation "https://andersfugmann.github.io/amqp-client/index.html")

(package
 (name amqp-client)
 (synopsis "Amqp client base library")
 (documentation "https://andersfugmann.github.io/amqp-client/index.html")
 (description
  "This library provides high level client bindings for amqp. The library is tested with rabbitmq, but should work with other amqp servers. The library is written in pure OCaml.\n\nThis is the base library required by lwt/async versions. You should install either amqp-client-async or amqp-client-lwt for actual client functionality.")
 (depends
  (ocaml (>= 4.14.0))
  (ezxmlm :build)
  (async :with-test)
  (lwt :with-test))
 (maintenance_intent "(latest)"))

(package
 (name amqp-client-lwt)
 (synopsis "Amqp client library, lwt version")
 (documentation "https://andersfugmann.github.io/amqp-client/index.html")
 (depends
  (ocaml (>= 4.14.0))
  (amqp-client (= :version))
  (lwt (>= 2.4.6))
  lwt_log
  uri)
 (maintenance_intent "(latest)"))

(package
 (name amqp-client-async)
 (synopsis "Amqp client library, async version")
 (documentation "https://andersfugmann.github.io/amqp-client/index.html")
 (depends
  (ocaml (>= 4.14.0))
  (amqp-client (= :version))
  (async (>= v0.16.0))
  uri)
 (maintenance_intent "(latest)"))

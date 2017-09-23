rabbitmq-c: rabbitmq-c.c
	gcc -o $@ $< -lrabbitmq

netamqp_test: netamqp_test.ml
	ocamlfind ocamlopt -package netamqp $< -o $@

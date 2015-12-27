.DEFAULT: all
.PHONY: $(MAKECMDGOALS)
all:
	omake -w -j 4



ifeq ($(MAKECMDGOALS), install)
OCAMLFINDDIR:=$(shell ocamlfind printconf destdir)

.PHONY: install install-lwt install-amqp
META.top: META
	cp $@ $<

# Recursive - one rule
install-lwt: export thread=lwt
install-lwt: DESTDIR=$(OCAMLFINDDIR)/amqp-client/$(thread)
install-lwt:
	mkdir -p $(DESTDIR)
	omake clean
	omake -j4 _build/amqp-client.cma _build/amqp-client.cmxa
	# Install the the lwt specifics
	cp _build/amqp-client.cma _build/amqp-client.cmxa _build/*.cmt _build/*.cmi _build/*.mli $(DESTDIR)

install-async: export thread=async
install-async: DESTDIR=$(OCAMLFINDDIR)/amqp-client/$(thread)
install-async:
	mkdir -p $(DESTDIR)
	omake clean
	omake -j4 _build/amqp-client.cma _build/amqp-client.cmxa
	# Install the the lwt specifics
	cp _build/amqp-client.cma _build/amqp-client.cmxa _build/*.cmt _build/*.cmi _build/*.mli $(DESTDIR)

# Determine targets to install
LIBS := $(addprefix install-, $(shell ocamlfind list | grep -E '^(async |lwt )' | cut -d' ' -f1))
install: DESTDIR=$(OCAMLFINDDIR)/amqp-client
install: $(LIBS)
	mkdir -p $(DESTDIR)
	cp META $(DESTDIR)

else ifeq ($(MAKECMDGOALS), uninstall)
.PHONY: uninstall
uninstall:
	@D=$$(ocamlfind query amqp-client) && echo rm -fr $${D}

else
$(MAKECMDGOALS):
	omake -w -j 4 $(MAKECMDGOALS)
endif

RELEASE = false
NPROC := $(shell nproc || echo 4)
OMAKE := omake -j $(NPROC) RELEASE=$(RELEASE)

.DEFAULT: all
.PHONY: $(MAKECMDGOALS)
all:
	$(OMAKE) -w


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
	$(OMAKE) clean
	$(OMAKE) _build/amqp-client.cma _build/amqp-client.cmxa _build/amqp-client.a
	cp _build/amqp-client.cma _build/amqp-client.cmxa  _build/amqp-client.a _build/*.cmt _build/*.cmi _build/*.mli _build/*.cmx $(DESTDIR)

install-async: export thread=async
install-async: DESTDIR=$(OCAMLFINDDIR)/amqp-client/$(thread)
install-async:
	mkdir -p $(DESTDIR)
	$(OMAKE) clean
	$(OMAKE) _build/amqp-client.cma _build/amqp-client.cmxa _build/amqp-client.a
	cp _build/amqp-client.cma _build/amqp-client.cmxa  _build/amqp-client.a _build/*.cmt _build/*.cmi _build/*.mli _build/*.cmx $(DESTDIR)

# Determine targets to install
LIBS := $(addprefix install-, $(shell ocamlfind list | grep -E '^(async |lwt )' | cut -d' ' -f1))
install: DESTDIR=$(OCAMLFINDDIR)/amqp-client
install: $(LIBS)
	mkdir -p $(DESTDIR)
	cp META $(DESTDIR)

else ifeq ($(MAKECMDGOALS), uninstall)
.PHONY: uninstall
uninstall:
	@D=$$(ocamlfind query amqp-client) && rm -fr $${D}

else
$(MAKECMDGOALS):
	$(OMAKE) -w $(MAKECMDGOALS)
endif

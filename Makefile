NPROC := $(shell nproc || echo 4)
OMAKE := omake -j $(NPROC)
OCAMLFINDDIR = $(shell ocamlfind printconf destdir)

LOCAL_TARGETS = all install uninstall test test-link

# Nothing is made by this makefile.
.PHONY: $(MAKECMDGOALS)

$(filter-out $(LOCAL_TARGETS), $(MAKECMDGOALS)):
	$(OMAKE) -w $(MAKECMDGOALS)

.DEFAULT: all
all:
	$(OMAKE) -w

.PHONY: install install-lwt install-amqp test test-link
test-link:
	thread=lwt $(OMAKE)   _build/lwt_link
	thread=async $(OMAKE) _build/async_link

test: test-link
	$(OMAKE) test

META.top: META
	cp $@ $<

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
install:
install: DESTDIR=$(OCAMLFINDDIR)/amqp-client
install: $(LIBS)
	mkdir -p $(DESTDIR)
	cp META $(DESTDIR)

uninstall:
	@D=$$(ocamlfind query amqp-client) && rm -fr $${D}

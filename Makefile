NPROC := $(shell nproc || echo 4)
.PHONY: all build clean test install update-version
all: build

build:
	jbuilder build @install -j $(NPROC) --dev

clean:
	jbuilder clean

test:
	jbuilder runtest -j $(NPROC)

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

# Run tests.
tests/%.exe: tests/%.ml
	jbuild build $@

integration:
	$(MAKE) clean
	sed 's/TYPE/async/g' tests/jbuild.in > tests/jbuild
	jbuilder runtest --dev
	$(MAKE) clean
	sed 's/TYPE/lwt/g' tests/jbuild.in > tests/jbuild
	jbuilder runtest --dev
	$(RM) tests/jbuild

update-version: VERSION=$(shell head -n 1 Changelog | sed 's/://')
update-version:
	@echo "Set version to: $(VERSION)"
	@sed -i 's/version = ".*"/version = "$(VERSION)"/' lib/amqp_connection.ml
	@sed -i 's/^version: ".*"/version: "$(VERSION)"/' amqp-client.opam

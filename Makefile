NPROC := $(shell nproc || echo 4)
.PHONY: all build clean test install update-version doc commit-doc
all: build

build:
	jbuilder build @install -j $(NPROC) --dev

clean:
	jbuilder clean
	rm -f test/jbuild

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
	jbuilder build @tests/integration --dev
	$(MAKE) clean
	sed 's/TYPE/lwt/g' tests/jbuild.in > tests/jbuild
	jbuilder build @tests/integration --dev

update-version: VERSION=$(shell head -n 1 Changelog | sed 's/://')
update-version:
	@echo "Set version to: $(VERSION)"
	@sed -i 's/version = ".*"/version = "$(VERSION)"/' async/connection.ml
	@sed -i 's/^version: ".*"/version: "$(VERSION)"/' amqp-client.opam

doc:
	jbuilder build --dev @doc

commit-doc: doc
	git -C html rm -fr \*
	cp -av _build/default/_doc/* html
	git -C html add '*' && echo ok
	git -C html commit --amend -m "Update documentation"
	git -C html push --force

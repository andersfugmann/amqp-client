NPROC := $(shell nproc || echo 4)
.PHONY: all build clean test install
all: build

build:
	jbuilder build @install -j $(NPROC)

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
	jbuilder runtest
	$(MAKE) clean
	sed 's/TYPE/lwt/g' tests/jbuild.in > tests/jbuild
	jbuilder runtest
	$(RM) tests/jbuild

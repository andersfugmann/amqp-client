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

.PHONY: all build clean test install
all: build

build:
	jbuilder build @install

clean:
	jbuilder clean

test:
	jbuilder runtest

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

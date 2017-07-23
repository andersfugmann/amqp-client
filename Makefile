all: build
gen_spec:
	jbuilder build tools/gen_spec.exe

build:
	jbuilder build

clean:
	jbuilder clean

all:
	omake -w -j 4
clean:
	$(RM) -r _build	OMake*.omc
	find . -name \*~ -delete
	$(RM) -r .omake*

.PHONY: test
test:
	omake -w -j 4 _build/test
	OCAMLRUNPARAM=b _build/test

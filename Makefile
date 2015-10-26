.PHONY: all clean test
all:
	omake -w -j 4
clean:
	$(RM) -r _build	OMake*.omc
	find . -name \*~ -delete
	$(RM) -r .omake*

test:
	omake -w -j 4 integration

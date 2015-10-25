all:
	omake -w -j 4
clean:
	$(RM) -r _build	OMake*.omc
	find . -name \*~ -delete
	$(RM) -r .omake*

.PHONY: $(MAKECMDGOALS)
$(MAKECMDGOALS):
	omake -w -j 4 $(MAKECMDGOALS)

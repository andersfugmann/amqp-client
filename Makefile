.DEFAULT: all
.PHONY: $(MAKECMDGOALS)
all:
	omake -w -j 4

$(MAKECMDGOALS):
	omake -w -j 4 $(MAKECMDGOALS)

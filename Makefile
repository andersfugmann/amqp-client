.DEFAULT: all
all:
	omake -w -j 4

$(MAKECMDGOALS):
	omake -w -j 4 $(MAKECMDGOALS)

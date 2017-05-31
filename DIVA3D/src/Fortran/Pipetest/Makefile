# Makefile to compile the Diva code

sources := $(wildcard *.f)
executables := $(patsubst %.f,%.a,$(sources)) 

all : $(executables)

%.a : %.f
	$(compiler) $(flags) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



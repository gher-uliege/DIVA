# Makefile to compile the Diva code

sources := $(wildcard *.f *.f90)
executables := $(addprefix $(BINDIR),diva.a) 

all : $(executables)

$(executables): | $(BINDIR)

$(BINDIR) :
	mkdir $(BINDIR)

$(executables) : $(sources) divainc.h divapre.h
	$(compiler) $(flags) $(flagscalc) $(sources) -o $@ 

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY

#FCFLAGSCALC=-O3 -cpp -DDIVAITERATIVE -fbounds-check -DDIVAHUGEMEMORY
FCFLAGSCALC=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY

BINDIR := ../../../bin/
	
sources := $(wildcard *.f *.f90)
executables := $(addprefix $(BINDIR),diva.a) 

all : $(executables)

$(executables): | $(BINDIR)

$(BINDIR) :
	mkdir $(BINDIR)

$(executables) : $(sources) divainc.h divapre.h
	$(FC) $(FCFLAGS) $(FCFLAGSCALC) $(sources) -o $@ 

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



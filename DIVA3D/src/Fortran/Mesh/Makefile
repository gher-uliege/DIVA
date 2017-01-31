# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAHUGEMEMORY
#FCFLAGS=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY

BINDIR := ../../../bin/
	
sources := $(wildcard *.f)
generopt := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(wildcard generopt*.f)))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(sources))) 

all : $(executables)

$(BINDIR)contourcheck.a $(generopt): iodv.h

$(executables): | $(BINDIR)

$(BINDIR):
	mkdir $(BINDIR)
	
$(executables) : $(BINDIR)%.a : %.f
	$(FC) $(FCFLAGS) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables) 



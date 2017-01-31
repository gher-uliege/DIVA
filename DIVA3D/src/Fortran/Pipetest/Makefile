# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAHUGEMEMORY
#FCFLAGS=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
#BINDIR := ../../../bin/
	
sources := $(wildcard *.f)
executables := $(patsubst %.f,%.a,$(sources)) 

all : $(executables)

%.a : %.f
	$(FC) $(FCFLAGS) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



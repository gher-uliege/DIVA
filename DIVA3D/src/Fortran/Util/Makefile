# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp 

LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-I/usr/local/include

BINDIR := ../../../bin/
	
sources := $(wildcard *.f *.f90)
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.f90,%.a,$(sources)))) 

all : $(executables)  

test:
	echo $(sources)

$(executables): | $(BINDIR)

$(BINDIR):
	mkdir $(BINDIR)

$(BINDIR)%.a : %.f
	$(FC) $(FCFLAGS) -o $@ $< 

$(BINDIR)%.a : %.f90
	$(FC) $(FCFLAGS) -o $@ $< 

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



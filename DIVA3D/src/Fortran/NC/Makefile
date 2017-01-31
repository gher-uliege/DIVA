# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
#FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY
FCFLAGS=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
LDFLAGS=-L/usr/lib -lnetcdff -lnetcdf
#LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-g -O2 -I/usr/include

BINDIR := ../../../bin/
	
sources := $(wildcard *.f *F)
sources90 := $(filter-out divaio.F90,$(wildcard *.f90 *F90))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.F,%.a,$(sources)))) 
executables90 := $(addprefix $(BINDIR),$(patsubst %.f90,%.a,$(patsubst %.F90,%.a,$(sources90))))

all : divaio.o $(executables) $(executables90)  

divaio.o : divaio.F90
	$(FC) $(ncinc) -c divaio.F90

$(executables): | $(BINDIR)

$(BINDIR):
	        mkdir $(BINDIR)

$(BINDIR)%.a : %.f
	$(FC) -o $@ $< $(LDFLAGS)

$(BINDIR)%.a : %.F
	$(FC) -o $@ $< $(LDFLAGS)

$(BINDIR)%.a : %.f90 divaio.o
	$(FC) $(ncinc) $(nocyg) $< -o $@ divaio.o $(LDFLAGS)

$(BINDIR)%.a : %.F90 divaio.o
	$(FC) $(ncinc) $(nocyg) $< -o $@ divaio.o $(LDFLAGS)

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o *.mod

cleanexec :
	rm $(executables) $(executables90)



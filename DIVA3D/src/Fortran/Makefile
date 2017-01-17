# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORYa
#FCFLAGS=-O2

LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-I/usr/local/include

BINDIR=../../../bin/

ureadwrite := ureadc.f uwritc.f
sources := $(filter-out $(ureadwrite),$(wildcard *.f))
objects := $(patsubst %.f,%.o,$(sources))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(sources))) 

all : ureadc.o uwritc.o $(executables)

ureadc.o : ureadc.f
	$(FC) -c $(FCFLAGS) ureadc.f -o ureadc.o
uwritc.o : uwritc.f
	$(FC) -c $(FCFLAGS) uwritc.f -o uwritc.o
extract.a : extract.f
	$(FC) extract.f -o extract.a $(LDFLAGS)
$(executables) : $(sources)
	$(FC) $(FCFLAGS) $< -o $@

# Utility targets
.PHONY: clean
clean:
	rm -f *.o *.mod *.MOD *.a


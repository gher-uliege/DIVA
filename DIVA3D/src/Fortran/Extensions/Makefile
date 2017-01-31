# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY

LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-I/usr/local/include

BINDIR := ../../../bin/
	
ureadwrite := ureadc.f uwritc.f
sources := $(filter-out $(ureadwrite) extract.f,$(wildcard *.f))
objects := $(patsubst %.f,%.o,$(ureadwrite))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(sources))) 

all : $(objects) $(executables) $(BINDIR)extract.a

$(executables): | $(BINDIR)

$(BINDIR):
	mkdir $(BINDIR)
	
ureadc.o : ureadc.f
	$(FC) -c $(FCFLAGS) ureadc.f -o ureadc.o

uwritc.o : uwritc.f
	$(FC) -c $(FCFLAGS) uwritc.f -o uwritc.o

$(BINDIR)extract.a : extract.f
	$(FC) extract.f -o $(BINDIR)extract.a $(LDFLAGS)

$(executables) : $(BINDIR)%.a : %.f
	$(FC) $(FCFLAGS) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



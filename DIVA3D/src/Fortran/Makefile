
# Pass variables from the top-level make
# (Variables are not normally passed down)

DIVA_PARALLEL=1

export compiler=gfortran


export flags=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -fallow-argument-mismatch 
ifeq ($(DIVA_PARALLEL), 1)
  export flagscalc=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY -fallow-argument-mismatch 
else
  export flagscalc=-O3 -cpp -DDIVAITERATIVE -Wall -fbounds-check -DDIVAHUGEMEMORY -fallow-argument-mismatch 
endif


export nclib=$(shell nf-config --flibs)
export ncinc=$(shell nf-config --fflags) -fallow-argument-mismatch

#export ncinc=-I/usr/local/include
#export nclib=-L/usr/local/lib -lnetcdff -L/usr/local/lib -lnetcdf -lnetcdf

# For ifort (adapt path to ifort compiled netCDF library)
#export compiler=ifort
#export flagscalc='-O3 -xHost -ip -no-prec-div -static-intel -cpp -DDIVAITERATIVE -Wall -fbounds-check'
#export ncinc=-I/home/ctroupin/Software/netCDF/include
#export nclib=-L/home/ctroupin/Software/netCDF/lib -lnetcdff -L/home/ctroupin/Software/netCDF/lib -lnetcdf -lnetcdf

# export nocyg=...
# export noglobals=...

export SRCDIR=$(PWD)
export BINDIR=$(SRCDIR)/../../bin/
export DIVA2DDIR=$(SRCDIR)/../../divastripped
export DIVAWORKDIR=$(DIVA2DDIR)/divawork/
export DIVAMESHDIR=$(DIVA2DDIR)/meshgenwork/
export DIVAINPUTDIR=$(DIVA2DDIR)/input/
export DIVAOUTPUTDIR=$(DIVA2DDIR)/output/
export DIVANCDIR=$(DIVA2DDIR)/output/ghertonetcdf/
export DIVAMESHVISUDIR=$(DIVA2DDIR)/output/meshvisu/


export MKDIR=mkdir -p
PLPLOT = Plplot
PLPLOT = NoPlplot   # Comment this line of you have Plplot installed


.PHONY: all calc extensions mesh netcdf pipetest stabil util directories cleanobject cleanexec
 
all: calc extensions mesh netcdf pipetest stabil util directories

test:
	@echo $(PWD)
	@echo "nclib: " $(nclib)
	@echo "ncinc: " $(ncinc)

directories: $(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR) $(BINDIR)

$(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR) $(BINDIR):
	$(MKDIR) $@

calc: 
	$(MAKE) -C Calc

extensions: 
	$(MAKE) -C Extensions 

mesh:
	$(MAKE) -C Mesh

netcdf: 
	$(MAKE) -C NC

pipetest: 
	$(MAKE) -C Pipetest

stabil: 
	$(MAKE) -C Stabil
	
util:
	$(MAKE) -C Util
       
# Utility targets

clean: cleanexec cleanobject

cleanexec:
	rm -f $(BINDIR)*.a

cleanobject:
	rm -f */*.o */*.mod


export compiler=/usr/bin/gfortran
export flags=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY
export flagscalc=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
export ncinc=-g -O2 -I/usr/local/include
export nclib=-L/usr/local/lib -lnetcdf -lnetcdff

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

MKDIR = mkdir -p

PLPLOT=Plplot
PLPLOT=NoPlplot   # Comment this line of you have Plplot installed

# Maybe add loop on subdirectories to be sure 
# we take them all into account
all: calc extensions mesh netcdf plplot pipetest stabil util

test:
	echo $(PWD)

directories: | $(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR)

$(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR):
	$(MKDIR) $@

$(BINDIR):
	$(MKDIR) $@

calc: | directories
	cd Calc && $(MAKE) all
	cd $(SRCDIR)

extensions: | directories
	cd Extensions && $(MAKE) all
	cd $(SRCDIR)

mesh: | directories
	cd Mesh && $(MAKE) all
	cd $(SRCDIR)

netcdf: | directories
	cd NC && $(MAKE) all
	cd $(SRCDIR)

plplot: | directories
	cd $(PLPLOT) && $(MAKE) all
	cd $(SRCDIR)

pipetest: | directories
	cd Pipetest && $(MAKE) all
	cd $(SRCDIR)

stabil: | directories
	cd Stabil && $(MAKE) all
	cd $(SRCDIR)
	
util: | directories
	cd Util && $(MAKE) all
	cd $(SRCDIR)
       
# Utility targets
.PHONY: cleanobject cleanexec cleanall

clean: cleanexec cleanobject

cleanexec:
	rm -f $(BINDIR)*.a

cleanobject:
	rm -f */*.o */*.mod

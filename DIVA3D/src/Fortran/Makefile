
# Pass variables from the top-level make
# (Variables are not normally passed down)
export compiler = /usr/bin/gfortran
export flags=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE
export flagscalc=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
export ncinc=-I/usr/local/include
export nclib=-L/usr/local/lib -lnetcdff -L/usr/local/lib -lnetcdf -lnetcdf
#export $(eval nclib=$(nf-config --flibs))

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


.PHONY: all directories cleanobject cleanexec
 
all: calc extensions mesh netcdf plplot pipetest stabil util directories

test:
	echo $(PWD)

directories: $(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR) $(BINDIR)

$(DIVAWORKDIR) $(DIVAMESHDIR) $(DIVAINPUTDIR) $(DIVAOUTPUTDIR) $(DIVANCDIR) $(DIVAMESHVISUDIR) $(BINDIR):
	$(MKDIR) $@

$(BINDIR):
	$(MKDIR) $@

calc: 
	$(MAKE) -C Calc

extensions: 
	$(MAKE) -C Extensions 

mesh:
	$(MAKE) -C Mesh

netcdf: 
	$(MAKE) -C NC

plplot: 
	$(MAKE) -C $(PLPLOT) 

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

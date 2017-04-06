
export compiler=/usr/bin/gfortran
export flags=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY
export flagscalc=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
export ncinc=-g -O2 -I/usr/local/include
export nclib=-L/usr/local/lib -lnetcdf -lnetcdff

# export nocyg=...
# export noglobals=...

export SRCDIR=$(PWD)
export BINDIR=$(SRCDIR)/../../bin/

PLPLOT=Plplot
PLPLOT=NoPlplot   # Comment this line of you have Plplot installed

# Maybe add loop on subdirectories to be sure 
# we take them all into account
all: calc extensions mesh netcdf plplot pipetest stabil util

test:
	echo $(PWD)
calc:   
	cd Calc && $(MAKE) all
	cd $(SRCDIR)

extensions:
	cd Extensions && $(MAKE) all
	cd $(SRCDIR)

mesh:
	cd Mesh && $(MAKE) all
	cd $(SRCDIR)

netcdf: 
	cd NC && $(MAKE) all
	cd $(SRCDIR)

plplot:
	cd $(PLPLOT) && $(MAKE) all
	cd $(SRCDIR)

pipetest:
	cd Pipetest && $(MAKE) all
	cd $(SRCDIR)

stabil:
	cd Stabil && $(MAKE) all
	cd $(SRCDIR)
	
util:
	cd Util && $(MAKE) all
	cd $(SRCDIR)
       

# Utility targets
.PHONY: cleanobject cleanexec cleanall

cleanall: cleanexec cleanobject

cleanexec:
	rm -v $(BINDIR)*.a

cleanobject:
	rm */*.o */*.mod

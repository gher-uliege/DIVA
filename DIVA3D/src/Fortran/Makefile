
LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-I/usr/local/include


BINDIR=../../bin/
SRCDIR=$(pwd)
PLPLOT=Plplot
PLPLOT=NoPlplot   # Comment this line of you have Plplot installed

# Maybe add loop on subdirectories to be sure 
# we take them all into account
all: calc extensions mesh netcdf plplot pipetest stabil util

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
	mv Stabil/*.a $(BINDIR)
	
util:
	cd Util && $(MAKE) all
	cd $(SRCDIR)
       

# Utility targets
.PHONY: cleanobject cleanexec cleanall

cleanall: cleanexec cleanobject

cleanexec:
	rm $(BINDIR)*.a

cleanobject:
	rm */*.o

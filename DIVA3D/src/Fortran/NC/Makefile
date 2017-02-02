# Makefile to compile the Diva code

sources := $(wildcard *.f *F)
sources90 := $(filter-out divaio.F90,$(wildcard *.f90 *F90))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.F,%.a,$(sources)))) 
executables90 := $(addprefix $(BINDIR),$(patsubst %.f90,%.a,$(patsubst %.F90,%.a,$(sources90))))

all : divaio.o $(executables) $(executables90)  

divaio.o : divaio.F90
	$(compiler) $(ncinc) $(nocyg) -c divaio.F90

$(executables): | $(BINDIR)

$(BINDIR):
	        mkdir $(BINDIR)

$(BINDIR)%.a : %.f
	$(compiler) $(nocyg) -o $@ $< $(nclib)

$(BINDIR)%.a : %.F
	$(compiler) $(nocyg) -o $@ $< $(nclib)

$(BINDIR)%.a : %.f90 divaio.o
	$(compiler) $(ncinc) $(nocyg) $< -o $@ divaio.o $(nclib)

$(BINDIR)%.a : %.F90 divaio.o
	$(compiler) $(ncinc) $(nocyg) $< -o $@ divaio.o $(nclib)

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o *.mod

cleanexec :
	rm $(executables) $(executables90)



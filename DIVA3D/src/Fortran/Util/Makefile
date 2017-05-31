# Makefile to compile the Diva code

sources := $(wildcard *.f *.f90)
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.f90,%.a,$(sources)))) 

all : $(executables)  

$(executables): | $(BINDIR)

$(BINDIR):
	$(MKDIR) $(BINDIR)

$(BINDIR)%.a : %.f
	$(compiler) $(flags) -o $@ $< 

$(BINDIR)%.a : %.f90
	$(compiler) $(flags) -o $@ $< 

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



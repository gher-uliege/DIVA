# Makefile to compile the Diva code

sources := $(wildcard *.f)
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(sources))) 

all : $(executables)

$(executables): | $(BINDIR)

$(BINDIR):
	$(MKDIR) $(BINDIR)
	
$(executables) : $(BINDIR)%.a : %.f
	$(compiler) $(flags) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)



# Makefile to compile the Diva code

ureadwrite := ureadc.f uwritc.f
sources := $(filter-out $(ureadwrite) extract.f,$(wildcard *.f))
objects := $(patsubst %.f,%.o,$(ureadwrite))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(sources))) 

all : $(objects) $(executables) $(BINDIR)extract.a

$(executables): | $(BINDIR)

$(BINDIR):
	$(MKDIR) $(BINDIR)
	
ureadc.o : ureadc.f
	$(compiler) -c $(flags) ureadc.f -o ureadc.o

uwritc.o : uwritc.f
	$(compiler) -c $(flags) uwritc.f -o uwritc.o

$(BINDIR)extract.a : extract.f
	$(compiler) $(noglobals) $(nocyg) extract.f -o $(BINDIR)extract.a $(nclib) $(ncinc)

$(executables) : $(BINDIR)%.a : %.f
	$(compiler) $(nocyg) $(nclib) $< -o $@

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(executables)


# Makefile to compile the Diva code

sources := $(wildcard *.f *F)
sources90 := $(filter-out divaio.F90,$(wildcard *.f90 *F90))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.F,%.a,$(sources)))) 
executables90 := $(addprefix $(BINDIR),$(patsubst %.f90,%.a,$(patsubst %.F90,%.a,$(sources90))))

all : $(executables) $(executables90) $(BINDIR)addnoise.a $(BINDIR)diva3Dqcwrt.a\
  $(BINDIR)diva3Dstr.a $(BINDIR)diva4Dqcwrt.a $(BINDIR)diva4Dstr.a $(BINDIR)dv4DYRstr.a \
  $(BINDIR)refdispach.a $(BINDIR)refenoise.a $(BINDIR)sortall.a $(BINDIR)sortref.a

$(executables): | $(BINDIR)

$(BINDIR):
	$(MKDIR) $(BINDIR)

../NC/divaio.o : ../NC/divaio.F90
	        $(compiler) $(ncinc) -c $^ -o $@

$(BINDIR)test_inpolygon.a : test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@ $(nclib)

$(BINDIR)diva3Dstr.a : diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@ $(nclib)

$(BINDIR)diva4Dstr.a : diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@ $(nclib)

$(BINDIR)dv4DYRstr.a : dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@ $(nclib)

$(BINDIR)diva3Dref.a : diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@ $(nclib)

$(BINDIR)stabil.a : stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F
	$(compiler) $(flags) $(ncinc) $^ -o $@ $(nclib)

$(BINDIR)dv3Dtswrt.a : dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@  $(nclib)

$(BINDIR)datatrans.a : datatrans.F transfun.F userfunc.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)refetrans.a : refetrans.F transfun.F userfunc.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)refebintrans.a : refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)anatrans.a : anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F
	$(compiler) $(flags) $(ncinc) $^ -o $@  $(nclib)

$(BINDIR)diva3Dqcwrt.a : diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@  $(nclib)

$(BINDIR)diva4Dqcwrt.a : diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC $^ -o $@  $(nclib)

$(BINDIR)fillfilavref.a : fillfilavref.F ureadc.F uwritc.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)addnoise.a : datagrpnoise.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)sortall.a : datasorts.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)refenoise.a : refegrnoise.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)sortref.a : refesorts.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)datapre.a : datapre.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)wclfile.a : wclfile.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)wsnfile.a : wsnfile.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)wgcvfile.a : wgcvfile.F
	$(compiler) $(flags) $^ -o $@ 

$(BINDIR)boundval.a : boundval.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)gebprep.a : gebprep.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)datamix.a : datamix.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)datadispach.a : datadispach.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)refdispach.a : refedispach.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)datacalmoy.a : datacalmoy.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)dvqcfiles.a : dvqcfiles.F
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)main_tomvec.a : main_tomvec.f90 tomvec.f
	$(compiler) $(flags) $(flags) $^ -o $@ 

$(BINDIR)binning_lines.a : binning_lines.f90
	$(compiler) $(flags) $(flags) $^ -o $@ 


# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(BINDIR)*.a



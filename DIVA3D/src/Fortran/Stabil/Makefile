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
	mkdir $(BINDIR)

../NC/divaio.o : ../NC/divaio.F90
	        $(compiler) $(ncinc) -c $^ -o $@

$(BINDIR)test_inpolygon.a : test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o -o $(BINDIR)test_inpolygon.a $(nclib)

$(BINDIR)diva3Dstr.a : diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o $(BINDIR)diva3Dstr.a $(nclib)

$(BINDIR)diva4Dstr.a : diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o $(BINDIR)diva4Dstr.a $(nclib)

$(BINDIR)dv4DYRstr.a : dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o -o $(BINDIR)dv4DYRstr.a $(nclib)

$(BINDIR)diva3Dref.a : diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o -o $(BINDIR)diva3Dref.a $(nclib)

$(BINDIR)stabil.a : stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F
	$(compiler) $(flags) $(ncinc) stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F -o $(BINDIR)stabil.a $(nclib)

$(BINDIR)dv3Dtswrt.a : dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o -o $(BINDIR)dv3Dtswrt.a $(nclib)

$(BINDIR)datatrans.a : datatrans.F transfun.F userfunc.F
	$(compiler) $(flags) datatrans.F transfun.F userfunc.F -o $(BINDIR)datatrans.a

$(BINDIR)refetrans.a : refetrans.F transfun.F userfunc.F
	$(compiler) $(flags) refetrans.F transfun.F userfunc.F -o $(BINDIR)refetrans.a

$(BINDIR)refebintrans.a : refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F
	$(compiler) $(flags) refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F -o $(BINDIR)refebintrans.a

$(BINDIR)anatrans.a : anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F
	$(compiler) $(flags) $(ncinc) anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F -o $(BINDIR)anatrans.a $(nclib)

$(BINDIR)diva3Dqcwrt.a : diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o -o $(BINDIR)diva3Dqcwrt.a $(nclib)

$(BINDIR)diva4Dqcwrt.a : diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o
	$(compiler) $(flags) $(ncinc) -I ../NC diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o -o $(BINDIR)diva4Dqcwrt.a $(nclib)

$(BINDIR)fillfilavref.a : fillfilavref.F ureadc.F uwritc.F
	$(compiler) $(flags) fillfilavref.F ureadc.F uwritc.F -o $(BINDIR)fillfilavref.a

$(BINDIR)addnoise.a : datagrpnoise.F
	$(compiler) $(flags) datagrpnoise.F -o $(BINDIR)addnoise.a

$(BINDIR)sortall.a : datasorts.F
	$(compiler) $(flags) datasorts.F -o $(BINDIR)sortall.a

$(BINDIR)refenoise.a : refegrnoise.F
	$(compiler) $(flags) refegrnoise.F -o $(BINDIR)refenoise.a

$(BINDIR)sortref.a : refesorts.F
	$(compiler) $(flags) refesorts.F -o $(BINDIR)sortref.a

$(BINDIR)datapre.a : datapre.F
	$(compiler) $(flags) datapre.F -o $(BINDIR)datapre.a

$(BINDIR)wclfile.a : wclfile.F
	$(compiler) $(flags) wclfile.F -o $(BINDIR)wclfile.a

$(BINDIR)wsnfile.a : wsnfile.F
	$(compiler) $(flags) wsnfile.F -o $(BINDIR)wsnfile.a

$(BINDIR)wgcvfile.a : wgcvfile.F
	$(compiler) $(flags) wgcvfile.F -o $(BINDIR)wgcvfile.a

$(BINDIR)boundval.a : boundval.F
	$(compiler) $(flags) boundval.F -o $(BINDIR)boundval.a

$(BINDIR)gebprep.a : gebprep.F
	$(compiler) $(flags) gebprep.F -o $(BINDIR)gebprep.a

$(BINDIR)datamix.a : datamix.F
	$(compiler) $(flags) datamix.F -o $(BINDIR)datamix.a

$(BINDIR)datadispach.a : datadispach.F
	$(compiler) $(flags) datadispach.F -o $(BINDIR)datadispach.a

$(BINDIR)refdispach.a : refedispach.F
	$(compiler) $(flags) refedispach.F -o $(BINDIR)refdispach.a

$(BINDIR)datacalmoy.a : datacalmoy.F
	$(compiler) $(flags) datacalmoy.F -o $(BINDIR)datacalmoy.a

$(BINDIR)dvqcfiles.a : dvqcfiles.F
	$(compiler) $(flags) dvqcfiles.F -o $(BINDIR)dvqcfiles.a

$(BINDIR)main_tomvec.a : main_tomvec.f90 tomvec.f
	$(compiler) $(flags) main_tomvec.f90 tomvec.f -o $(BINDIR)main_tomvec.a

$(BINDIR)binning_lines.a : binning_lines.f90
	$(compiler) $(flags) binning_lines.f90 -o $(BINDIR)binning_lines.a


# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(BINDIR)*.a



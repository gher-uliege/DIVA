# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY
#FCFLAGS=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
LDFLAGS=-L/usr/lib -lnetcdff -lnetcdf
ncinc=-g -O2 -I/usr/include

BINDIR := ../../../bin/
	
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
	        $(FC) $(ncinc) -c $^ -o $@

$(BINDIR)test_inpolygon.a : test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o -o $(BINDIR)test_inpolygon.a $(LDFLAGS)

$(BINDIR)diva3Dstr.a : diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o $(BINDIR)diva3Dstr.a $(LDFLAGS)

$(BINDIR)diva4Dstr.a : diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o $(BINDIR)diva4Dstr.a $(LDFLAGS)

$(BINDIR)dv4DYRstr.a : dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o -o $(BINDIR)dv4DYRstr.a $(LDFLAGS)

$(BINDIR)diva3Dref.a : diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o -o $(BINDIR)diva3Dref.a $(LDFLAGS)

$(BINDIR)stabil.a : stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F
	$(FC) $(FCFLAGS) $(ncinc) stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F -o $(BINDIR)stabil.a $(LDFLAGS)

$(BINDIR)dv3Dtswrt.a : dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o -o $(BINDIR)dv3Dtswrt.a $(LDFLAGS)

$(BINDIR)datatrans.a : datatrans.F transfun.F userfunc.F
	$(FC) $(FCFLAGS) datatrans.F transfun.F userfunc.F -o $(BINDIR)datatrans.a

$(BINDIR)refetrans.a : refetrans.F transfun.F userfunc.F
	$(FC) $(FCFLAGS) refetrans.F transfun.F userfunc.F -o $(BINDIR)refetrans.a

$(BINDIR)refebintrans.a : refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F -o $(BINDIR)refebintrans.a

$(BINDIR)anatrans.a : anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) $(ncinc) anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F -o $(BINDIR)anatrans.a $(LDFLAGS)

$(BINDIR)diva3Dqcwrt.a : diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o -o $(BINDIR)diva3Dqcwrt.a $(LDFLAGS)

$(BINDIR)diva4Dqcwrt.a : diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o -o $(BINDIR)diva4Dqcwrt.a $(LDFLAGS)

$(BINDIR)fillfilavref.a : fillfilavref.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) fillfilavref.F ureadc.F uwritc.F -o $(BINDIR)fillfilavref.a

$(BINDIR)addnoise.a : datagrpnoise.F
	$(FC) $(FCFLAGS) datagrpnoise.F -o $(BINDIR)addnoise.a

$(BINDIR)sortall.a : datasorts.F
	$(FC) $(FCFLAGS) datasorts.F -o $(BINDIR)sortall.a

$(BINDIR)refenoise.a : refegrnoise.F
	$(FC) $(FCFLAGS) refegrnoise.F -o $(BINDIR)refenoise.a

$(BINDIR)sortref.a : refesorts.F
	$(FC) $(FCFLAGS) refesorts.F -o $(BINDIR)sortref.a

$(BINDIR)datapre.a : datapre.F
	$(FC) $(FCFLAGS) datapre.F -o $(BINDIR)datapre.a

$(BINDIR)wclfile.a : wclfile.F
	$(FC) $(FCFLAGS) wclfile.F -o $(BINDIR)wclfile.a

$(BINDIR)wsnfile.a : wsnfile.F
	$(FC) $(FCFLAGS) wsnfile.F -o $(BINDIR)wsnfile.a

$(BINDIR)wgcvfile.a : wgcvfile.F
	$(FC) $(FCFLAGS) wgcvfile.F -o $(BINDIR)wgcvfile.a

$(BINDIR)boundval.a : boundval.F
	$(FC) $(FCFLAGS) boundval.F -o $(BINDIR)boundval.a

$(BINDIR)gebprep.a : gebprep.F
	$(FC) $(FCFLAGS) gebprep.F -o $(BINDIR)gebprep.a

$(BINDIR)datamix.a : datamix.F
	$(FC) $(FCFLAGS) datamix.F -o $(BINDIR)datamix.a

$(BINDIR)datadispach.a : datadispach.F
	$(FC) $(FCFLAGS) datadispach.F -o $(BINDIR)datadispach.a

$(BINDIR)refdispach.a : refedispach.F
	$(FC) $(FCFLAGS) refedispach.F -o $(BINDIR)refdispach.a

$(BINDIR)datacalmoy.a : datacalmoy.F
	$(FC) $(FCFLAGS) datacalmoy.F -o $(BINDIR)datacalmoy.a

$(BINDIR)dvqcfiles.a : dvqcfiles.F
	$(FC) $(FCFLAGS) dvqcfiles.F -o $(BINDIR)dvqcfiles.a

$(BINDIR)main_tomvec.a : main_tomvec.f90 tomvec.f
	$(FC) $(FCFLAGS) main_tomvec.f90 tomvec.f -o $(BINDIR)main_tomvec.a

$(BINDIR)binning_lines.a : binning_lines.f90
	$(FC) $(FCFLAGS) binning_lines.f90 -o $(BINDIR)binning_lines.a


# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(BINDIR)*.a



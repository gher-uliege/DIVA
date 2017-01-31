# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY
#FCFLAGS=-O3 -cpp -fopenmp -DDIVAITERATIVE -DDIVAPARALLEL -DDIVAHUGEMEMORY
LDFLAGS=-L/usr/local/lib -lnetcdff -lnetcdf
ncinc=-g -O2 -I/usr/include

BINDIR := ./
	
sources := $(wildcard *.f *F)
sources90 := $(filter-out divaio.F90,$(wildcard *.f90 *F90))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.F,%.a,$(sources)))) 
executables90 := $(addprefix $(BINDIR),$(patsubst %.f90,%.a,$(patsubst %.F90,%.a,$(sources90))))

all : $(executables) $(executables90) addnoise.a diva3Dqcwrt.a\
  diva3Dstr.a diva4Dqcwrt.a diva4Dstr.a dv4DYRstr.a \
  refdispach.a refenoise.a sortall.a sortref.a

$(executables): | $(BINDIR)

$(BINDIR):
	mkdir $(BINDIR)

../NC/divaio.o : ../NC/divaio.F90
	        $(FC) $(ncinc) -c $^ -o $@

test_inpolygon.a : test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o -o test_inpolygon.a $(LDFLAGS)

diva3Dstr.a : diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o diva3Dstr.a $(LDFLAGS)

diva4Dstr.a : diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o diva4Dstr.a $(LDFLAGS)

dv4DYRstr.a : dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o -o dv4DYRstr.a $(LDFLAGS)

diva3Dref.a : diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o -o diva3Dref.a $(LDFLAGS)

stabil.a : stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F
	$(FC) $(FCFLAGS) $(ncinc) stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F -o stabil.a $(LDFLAGS)

dv3Dtswrt.a : dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o -o dv3Dtswrt.a $(LDFLAGS)

datatrans.a : datatrans.F transfun.F userfunc.F
	$(FC) $(FCFLAGS) datatrans.F transfun.F userfunc.F -o datatrans.a

refetrans.a : refetrans.F transfun.F userfunc.F
	$(FC) $(FCFLAGS) refetrans.F transfun.F userfunc.F -o refetrans.a

refebintrans.a : refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F -o refebintrans.a

anatrans.a : anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) $(ncinc) anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F -o anatrans.a $(LDFLAGS)

diva3Dqcwrt.a : diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o -o diva3Dqcwrt.a $(LDFLAGS)

diva4Dqcwrt.a : diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o
	$(FC) $(FCFLAGS) $(ncinc) -I ../NC diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o -o diva4Dqcwrt.a $(LDFLAGS)

fillfilavref.a : fillfilavref.F ureadc.F uwritc.F
	$(FC) $(FCFLAGS) fillfilavref.F ureadc.F uwritc.F -o fillfilavref.a

addnoise.a : datagrpnoise.F
	$(FC) $(FCFLAGS) datagrpnoise.F -o addnoise.a

sortall.a : datasorts.F
	$(FC) $(FCFLAGS) datasorts.F -o sortall.a

refenoise.a : refegrnoise.F
	$(FC) $(FCFLAGS) refegrnoise.F -o refenoise.a

sortref.a : refesorts.F
	$(FC) $(FCFLAGS) refesorts.F -o sortref.a

datapre.a : datapre.F
	$(FC) $(FCFLAGS) datapre.F -o datapre.a

wclfile.a : wclfile.F
	$(FC) $(FCFLAGS) wclfile.F -o wclfile.a

wsnfile.a : wsnfile.F
	$(FC) $(FCFLAGS) wsnfile.F -o wsnfile.a

wgcvfile.a : wgcvfile.F
	$(FC) $(FCFLAGS) wgcvfile.F -o wgcvfile.a

boundval.a : boundval.F
	$(FC) $(FCFLAGS) boundval.F -o boundval.a

gebprep.a : gebprep.F
	$(FC) $(FCFLAGS) gebprep.F -o gebprep.a

datamix.a : datamix.F
	$(FC) $(FCFLAGS) datamix.F -o datamix.a

datadispach.a : datadispach.F
	$(FC) $(FCFLAGS) datadispach.F -o datadispach.a

refdispach.a : refedispach.F
	$(FC) $(FCFLAGS) refedispach.F -o refdispach.a

datacalmoy.a : datacalmoy.F
	$(FC) $(FCFLAGS) datacalmoy.F -o datacalmoy.a

dvqcfiles.a : dvqcfiles.F
	$(FC) $(FCFLAGS) dvqcfiles.F -o dvqcfiles.a

main_tomvec.a : main_tomvec.f90 tomvec.f
	$(FC) $(FCFLAGS) main_tomvec.f90 tomvec.f -o main_tomvec.a

binning_lines.a : binning_lines.f90
	$(FC) $(FCFLAGS) binning_lines.f90 -o binning_lines.a


# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(BINDIR)*.a



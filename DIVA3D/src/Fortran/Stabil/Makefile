# Makefile to compile the Diva code

# Compiler
FC=/usr/bin/gfortran
# flags for maximum performance or for debugging
FCFLAGS=-O3 -frecord-marker=4 -cpp -DDIVAITERATIVE -DDIVAHUGEMEMORY

LDFLAGS=-L/usr/local/lib -lnetcdff  -lnetcdf -lnetcdf
ncinc=-I/usr/local/include

BINDIR := ../../../bin/
	
sources := $(wildcard *.f *F)
sources90 := $(filter-out divaio.F90,$(wildcard *.f90 *F90))
executables := $(addprefix $(BINDIR),$(patsubst %.f,%.a,$(patsubst %.F,%.a,$(sources)))) 
executables90 := $(addprefix $(BINDIR),$(patsubst %.f90,%.a,$(patsubst %.F90,%.a,$(sources90))))

all : divaio.o $(executables) $(executables90)  

$(executables): | $(BINDIR)

$(BINDIR):
	        mkdir $(BINDIR)

test_inpolygon.a : test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC test_inpolygon.F90 dvreadfinal4D.F dvwritefinal4D.F ../NC/divaio.o -o test_inpolygon.a $nclib

diva3Dstr.a : diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC diva3Dwrt.F ureadc.F uwritc.F dv2Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o diva3Dstr.a $nclib

diva4Dstr.a : diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC diva4Dwrt.F dv3Dreadnc.F dv3Dncwrt.F ../NC/divaio.o -o diva4Dstr.a $nclib

dv4DYRstr.a : dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC dv4DYRwrt.F dv3Dreadnc.F dv3DncYRw.F ../NC/divaio.o -o dv4DYRstr.a $nclib

diva3Dref.a : diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC diva3Dref.F ureadc.F uwritc.F diva3Dnc.F ../NC/divaio.o -o diva3Dref.a $nclib

stabil.a : stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F
	$compiler $flags $ncinc stabil.F N2brunt.F albe2d.F potmp.F pzcon.F ureadc.F -o stabil.a $nclib

dv3Dtswrt.a : dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC dv3Dtswrt.F dvvarerr.F brunt.F AlpBetSig.F potmp.F pzcon.F dv3Dtsdnc.F ../NC/divaio.o -o dv3Dtswrt.a $nclib

datatrans.a : datatrans.F transfun.F userfunc.F
	$compiler $flags datatrans.F transfun.F userfunc.F -o datatrans.a

refetrans.a : refetrans.F transfun.F userfunc.F
	$compiler $flags refetrans.F transfun.F userfunc.F -o refetrans.a

refebintrans.a : transfun.F userfunc.F ureadc.F uwritc.F
	$compiler $flags refebintrans.F transfun.F userfunc.F ureadc.F uwritc.F -o refebintrans.a

anatrans.a : anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F
	$compiler $flags $ncinc anatrans.F dv2Dreadnc.F transfun.F userfunc.F dv2Drewtnc.F ureadc.F uwritc.F -o anatrans.a $nclib

diva3Dqcwrt.a : diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC diva3D_qc.F ureadc.F uwritc.F dv2Dreadnc.F diva_ncclim.F caldivanorm.F ../NC/divaio.o -o diva3Dqcwrt.a $nclib

diva4Dqcwrt.a : diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o
	$compiler $flags $ncinc -I ../NC diva4D_qc.F dv3Dreadnc.F diva_ncclim.F caldivanorm.F calrms.F ../NC/divaio.o -o diva4Dqcwrt.a $nclib

fillfilavref.a : fillfilavref.F ureadc.F uwritc.F
	$compiler $flags fillfilavref.F ureadc.F uwritc.F -o fillfilavref.a

addnoise.a : datagrpnoise.F
	$compiler $flags datagrpnoise.F -o addnoise.a

sortall.a : datasorts.F
	$compiler $flags datasorts.F -o sortall.a

refenoise.a : refegrnoise.F
	$compiler $flags refegrnoise.F -o refenoise.a

sortall.a : datasorts.F:
	$compiler $flags datasorts.F -o sortall.a

%.a : %.F
	$(FC) $(FCFLAGS) $< -o $@

#$compiler $flags datapre.F -o datapre.a
#$compiler $flags wclfile.F -o wclfile.a
#$compiler $flags wsnfile.F -o wsnfile.a
#$compiler $flags wgcvfile.F -o wgcvfile.a
#$compiler $flags boundval.F -o boundval.a
#$compiler $flags gebprep.F -o gebprep.a
#$compiler $flags datamix.F -o datamix.a
#$compiler $flags datadispach.F -o datadispach.a
#$compiler $flags refedispach.F -o refdispach.a
#$compiler $flags datacalmoy.F -o datacalmoy.a

# Utility targets
.PHONY: cleanall cleanobj cleanexec

cleanall : cleanobj cleanexec

cleanobj :
	rm *.o

cleanexec :
	rm $(BINDIR)*.a



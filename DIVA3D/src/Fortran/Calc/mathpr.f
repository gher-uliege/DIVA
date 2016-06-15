C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  MATHPR (MODULE)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             MATHPR MODULE                            C
C               Description of the mathematical problem                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine mathpr(ipr)
      include'divapre.h'
#include "divainc.h"
C
C  input of general data
C
      read(10,*) ityp
      if(ipr.gt.0) write(6,*) ' Finite Element type     =',ityp
      read(10,*) isym
      if(ipr.gt.0) write(6,*) ' Symetric problem : isym =',isym
      read(10,*) ipb
      if(ipr.gt.0) write(6,*) ' Problem type :      ipb =',ipb
C
C  PROBLEM P2: (see: BRASSEUR, Ph.D. Dissertation, 1993)
C
      if(ipb.eq.2) then
         read(12,*) alpha0
         read(12,*) alpha1
         if(ipr.gt.0) write(6,*) ' Parameter alpha0 =',alpha0
         if(ipr.gt.0) write(6,*) ' Parameter alpha1 =',alpha1
      endif
      return
      end

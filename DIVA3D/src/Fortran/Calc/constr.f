C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  CONSTR (MODULE)
C     -  RDPROP (READ THE NODAL PROPERTIES FOR CONSTRAINT IMPLEMENTATION
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             CONSTR MODULE                            C
C          Input of information for constraint implementation          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine constr(ipr)
      include'divapre.h'
#include "divainc.h"
C
C  READ GENERAL DATA
C
      nnpr=0
      read(10,*)itcs
      if(itcs.eq.2) then
       nnpr=1
       endif
       if(itcs.eq.1) then
       nnpr=2
       endif
       if(itcs.eq.3) then
       nnpr=3
      endif
      
      
      if(ipr.gt.0) write(6,*) ' Type of constraint =',itcs
      if(itcs.eq.1.and.ipb.ne.2) then
         write(6,*) ' %%% ERROR: ITSC=1 and IPB =/ 2 : NOT ALLOWED %%%'
         stop
      endif
      if(itcs.eq.1.or.itcs.eq.3) then
C JMB2012 added reading of third optional parameter decayrate
         decayrate=0
         visc=0
         theta=0
         read(49,*,END=6543,ERR=6543) theta,visc,decayrate
         goto 6544
 6543   continue
         rewind(49)
         read(49,*) theta,visc
 6544   continue
       if (theta.eq.0.) then
        write(6,*) ' Warning Constraint weight (theta)=',theta
        write(6,*) ' No advection constraint',theta
        itcs=0 
        return
       endif
       
      
         if(ipr.gt.0) write(6,*) ' Constraint weight (theta) =',theta
         if(ipr.gt.0) write(6,*) ' Constraint viscosity =',visc
         if(ipr.gt.0) write(6,*) ' Constraint decay rate =',decayrate
         if(ipr.gt.0) write(6,*) ' Number of nodal properties =',nnpr
       if(icoordchange.eq.1) then
        write(6,*) 'Coordinate change is active'
        write(6,*) 'Visc, vel, decay rate adapted to km scales'
C what counts is the reynolds number. 
        visc=visc/1000
C JMB2012 DECAY
        decayrate=decayrate*1000
       endif
       
       endif
       
C
C ALLOCATION OF STORAGE TABLES:
C  ==> TPROP(I,J)    : VALUE OF NODAL PROPERTY J at NODE I
C

       call allody(nnpr*nnt1,1,'tprop',ltprop,ipr)
       call rdprop(s(ltprop),s(ltcoog),ipr)
C read sources (if any)
C JMB2012 read source terms, if coordinate change multiply by 1000
       call sourcepr(ipr)
       if((icoordchange.eq.1).and.(NSOURCES.GT.0)) then
         call sourcescale(s(ltdataQ),NSOURCES)
       endif
       return
      
      end
CJMB2012 scale for km
      subroutine sourcescale(TTTT,NSOURCES)
      include'divapre.h'
      dimension TTTT(NSOURCES,4)
      do iii=1,NSOURCES
C      write(6,*) '???',TTTT(iii,3)
      TTTT(iii,3)=TTTT(iii,3)*1000
      enddo
      return
      end

      subroutine rdprop(tprop,tcoog,ipr)
C
C  I/O NODAL PROPERTIES FOR CONSTRAINT IMPLEMENTATION
C
      include'divapre.h'
#include "divainc.h"
      real*4 valexll,valexuu
      dimension tprop(nnt1,nnpr),tcoog(nnt1,2)
      if (itcs.eq.1.or.itcs.eq.3) then
      write(6,*) 'Advection constrained activated'
CJMB Initialize 
      call JMBCONSTR(tprop(1,1),tprop(1,2),tcoog(1,1),tcoog(1,2),0,
     &  icoordchange,valexuu)
C,dxkm,dykm)
      do 10 i=1,nnt1
        call JMBCONSTR(tprop(i,1),tprop(i,2),tcoog(i,1),tcoog(i,2),1,
     &  icoordchange,valexuu)
C,dxkm,dykm)
C         read(50,*) (tprop(i,j),j=1,nnpr)
c         if (tprop(i,1).ge.99.0.or.tprop(i,2).ge.99.0)then
c           tprop(i,1)=0.
c           tprop(i,2)=0.
c         
c         endif
 10   continue
C
C When Finished, deallocate working array
      call allody(-NSWSP,1,'WORKS',IPWSP,1)
C  OUTPUT OF B.C. DESCRIPTION
C
      if(ipr.ge.3) then
         write(6,*)' List of  nodes  and  NODAL PROPERTIES             '
         write(6,*)' --------------------------------------------------'
         do 100 i=1,nnt1
           write(6,*) i,(tprop(i,j),j=1,nnpr)
 100     continue
      endif

C      call propllxy(tprop,ipr)
CJMB changed to mean square
      umean=0.
      do 20 i=1,nnt1
          umean=umean+(tprop(i,1)*tprop(i,1)+tprop(i,2)*tprop(i,2))
 20   continue

C      write(6,*) 'umean ', umean
C JMB 2007 replaced alpha1 by 2/L^2 so that xi=0 remains possible
C
      if (umean.ne.0.) then
       umean=umean/nnt1
       if (rl0.eq.0) then
       rl0=sqrt(sqrt(1./alpha0))
       endif
       wc1=theta/(umean*RL0*RL0)
      else
       wc1=0.
       itcs=0 
       write(6,*) 'Warning: umean = 0'
      endif

      write(6,*) 'alpha1 = ', alpha1
      write(6,*) 'theta = ', theta
      write(6,*) 'constraint umean = ', sqrt(umean)
      write(6,*) 'Constraint weight wc1 = ',wc1
      write(6,*) 'Length scale',RL0
      endif
      
C VARIABLE L?
      if(itcs.eq.2.or.itcs.eq.3) then
      write(6,*) 'Variable L activated'
      if (itcs.eq.2) then
       JJPOS=1
                     else
       JJPOS=3
      endif
      
      
            write(6,*) 'Variable L activated'
CJMB Initialize 
      call JMBCONSTRL(tprop(1,JJPOS),tcoog(1,1),tcoog(1,2),0,
     &  icoordchange,valexll)
C,dxkm,dykm)
      do 1010 i=1,nnt1
        call JMBCONSTRL(tprop(i,JJPOS),tcoog(i,1),tcoog(i,2),1,
     &  icoordchange,valexll)
C,dxkm,dykm)
C         read(50,*) (tprop(i,j),j=1,nnpr)
         if (tprop(i,JJPOS).ge.99.0)then
           tprop(i,JJPOS)=1.
         endif
         if (tprop(i,JJPOS).le.0.0)then
           tprop(i,JJPOS)=1.
         endif

 1010   continue
C
C When Finished, deallocate working array
      call allody(-NSWSP,1,'WORKS',IPWSP,1)
C  OUTPUT OF B.C. DESCRIPTION
      
      
      
      
      endif
C End variable L
      return
      end
      subroutine JMBCONSTR(U,V,X,Y,II,icord,valexuu)
C,dxkm,dykm)
      include 'divapre.h'
#include "divainc.h"
      real*8 U,V,X,Y
      

      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf,xt,yt,UT,VT

      integer II,imaxf,jmaxf,icord
      integer imaxff,jmaxff
c      parameter(imaxff=500,jmaxff=500)
c      real*4 UF(imaxff,jmaxff),VF(imaxff,jmaxff)
      real*8 xx,yy,dxkm,dykm
      real*4 valexuu
      COMMON/JMBCON/x0f,dxxf,dyxf,y0f,dxyf,dyyf,UF,VF,imaxf,jmaxf,
     & ierruv
      U=0
      V=0
      
      if(II.EQ.0) then
      ierruv=0
C implement matrix reading
C Use units 91 92 (U,V) and 93(grid info)
      read(93,*,end=922,err=922) x0f
      read(93,*,end=922,err=922) y0f
      read(93,*,end=922,err=922) dxxf
      read(93,*,end=922,err=922) dyyf
      read(93,*,end=922,err=922) imaxf
      read(93,*,end=922,err=922) jmaxf
      
      x0f=x0f-dxxf
      y0f=y0f-dyyf
C
c      if(imaxf*jmaxf.gt.(imaxff*jmaxff)) then
c      write(6,*) 'increase imaxff*jmaxff to', imaxf*jmaxf
c      goto 922
c      endif
C Use allody and save pointer (to use allody with negative values afterwards?)
C 
      NSWSP=2*imaxf*jmaxf
C Try to read in simple precision in two fields (eq 1 double precision)
      NSWSP=imaxf*jmaxf+mod(imaxf*jmaxf,2)
      call allody(NSWSP,1,'WORKS',IPWSP,1)
      vrepl=0
      call uur(91,S(IPWSP),imaxf,jmaxf,valexuu,0)
c      write(6,*) 'NSWSP',NSWSP,NSWSP/2
      call uur(92,S(IPWSP+NSWSP/2),imaxf,jmaxf,valexuu,0)
C      call uur(91,UF,imaxf,jmaxf)
C      call uur(92,VF,imaxf,jmaxf)
      goto 923
 922  continue
      write(6,*) 'Error reading grid info for velocity constraint'
      U=0
      V=0
      ierruv=1
      return
 
C replace exclusion values by zero
C Need to change coordinates for x0 etc...

      write(6,*) 'Preparing constraint'
      write(6,*) 'Coordinate change',icord
CJMB TEST
 
 923  continue
C End TEST UF VF dyyf dxxf x0f y0f imaxff jmaxff = read in
      xx=x0f
      yy=y0f
      if (icord.ne.0) then
      call llxy(xx,yy)
      dxxf=dxxf*dxkm
      dyyf=dyyf*dykm
      endif
      x0f=xx
      y0f=yy
      
      dyxf=0
      dxyf=0
      
C
C
      return
      endif
      if(II.EQ.1) then
C 
      if(ierruv.eq.1) then
      U=0
      V=0
      return
      endif
      xt=X
      yt=Y
       
       call bilinin(S(IPWSP),S(IPWSP+NSWSP/2),
     &  x0f,dxxf,dyxf,y0f,dxyf,dyyf
     &                      ,imaxf,jmaxf,
     &                     UT,VT,xt,yt)
      U=UT
      if(icoordchange.lt.0) U=U*dxkm
      V=VT
C      write(6,*) 'X,Y',x,y,UT,VT
C This assumes we use velocities (in m/s). So of coordinates in degrees 
C normally coordinate change is required on x,y but not on u,v
C Should U,V be rather gradients, they must be gradients in the km space, since the are not modified.
C In other words, U,V should not be derivatives with respect to long/ lat.
      return
      endif
      end

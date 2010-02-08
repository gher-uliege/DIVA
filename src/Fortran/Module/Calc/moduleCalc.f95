MODULE moduleCalc

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module specifications               ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  ALLODY (dynamical allocation of storage area in S or L vector)


      SUBROUTINE ALLODY(NCOMP,IR,TAB,IDEB,IPR)
!C     ====================================
      include'divapre.h'
      INCLUDE'divainc.h'
      CHARACTER*5 TAB
      DATA ZERO/0.0D0/
      IF(IR.EQ.0) GO TO 20
!C
!C  STORAGE IN S VECTOR
!C
      IRE1=IRE+NCOMP
!C
!C  LENGTH TEST
!C
      IF(IRE1.LE.NREA) GO TO 10
!C#ifdef DIVADYNAMIC
       write(6,*) 'Dynamic reallocation'
       write(6,*) 'Might cause crash if not enough memory left'
       write(6,*) 'Momentarely doubles memory needed'

!C      allocate SN(IRE)
!C      copy S into SN
!C      deallocate S
!C      allocate S(IRE1)
!C      copy SN into S
!C      NREA=IRE1
!C      goto 10
!C#endif
      WRITE(6,400) TAB,IRE1,NREA
 400  FORMAT(/'  ** ERROR - ALLODY - STORAGE OF ',A5,/,' REQUIRED SPACE',I8,/'   AVAILABLE SPACE : ',I8)
      STOP
 10   IDEB=IRE+1
      IRE=IRE1
      IF(IRE.GT.IREMAX) IREMAX=IRE
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,401) TAB,IDEB,IRE
 401  FORMAT(2X,'ARRAY ',A5,' STORED IN S(',I8,') TO S(',I8,')')
!C
!C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
!C  unless call with ipr=99
!C

      DO 11 I=IDEB,IRE
 11   S(I)=ZERO
      RETURN
 20   CONTINUE
!C
!C  STORAGE IN L VECTOR
!C
      IEN1=IEN+NCOMP
!C
!C  LENGTH TEST
!C
      IF(IEN1.LE.NENT) GO TO 30
!C#ifdef DIVADYNAMIC

!C      allocate LN(IEN)
!C      copy L into LN
!C      deallocate L
!C      allocate L(IEN1)
!C      copy SN into S
!
!C      NENT=IEN1
!C      goto 30
!C#endif

      WRITE(6,400) TAB,IEN1,NENT
      STOP
 30   IDEB=IEN+1
      IEN=IEN1
      IF(IEN.GT.IENMAX) IENMAX=IEN
      if(IPR.EQ.99) RETURN
      IF(IPR.GE.2) WRITE(6,402) TAB,IDEB,IEN
 402  FORMAT(2X,'ARRAY ',A5,' STORED IN L(',I8,') TO L(',I8,')')
!C
!C  INITIALISATION TO ZERO OF THE NEWLY CREATED TABLE
!C
      
      DO 31 I=IDEB,IEN
 31   L(I)=0
      RETURN
      END SUBROUTINE



! Procedure 2
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  BCONDI (MODULE)
!C     -  RDCOND (READ THE BOUNDARY CONDITIONS)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             BCONDI MODULE                            C
!C             Dirichlet boundary conditions to be fixed                C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine bcondi(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  READ THE BOUNDARY CONDITIONS TO BE IMPLEMENTED
!C
      read(10,*)ncond
      if(ipr.gt.0) write(6,*) ' Number of Boundary Conditions =',ncond
      info=1
      if(ityp.eq.2.or.ityp.eq.3) info=2
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> TCNDI(I)    : VALUE OF BOUNDARY CONDITION I
!C  ==> KCNDI(I,*)  : NODE FOR BOUNDARY CONDITION
!C                    (IF ITYP.EQ.2 OR 3, ALSO CONNECTOR AT NODE !!!)
!C
      call allody(ncond,1,'tcndi',ltcndi,ipr)
      call allody(ncond*info,0,'kcndi',lkcndi,ipr)
      call rdcond(l(lkcndi),s(ltcndi),ipr)
      return
      end subroutine

! Procedure 3
! -----------

      subroutine rdcond(kcndi,tcndi,ipr)
!C
!C  I/O BOUNDARY CONDITIONS TO BE FIXED (ONLY DIRICHLET TYPE)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcndi(ncond),kcndi(ncond,info)
!C
!C  INPUT OF B.C. DESCRIPTION
!C
      do 10 i=1,ncond
         read(30,*) tcndi(i),(kcndi(i,j),j=1,info)
 10   continue
!C
!C  OUTPUT OF B.C. DESCRIPTION
!C
      if(ipr.ge.3) then
         write(6,*)' List of B.C. value,  nodes  and  d.o.f.           '
         write(6,*)' --------------------------------------------------'
         do 100 i=1,ncond
           write(6,*) tcndi(i),(kcndi(i,j),j=1,info)
 100     continue
      endif
      return
      end subroutine

! Procedure 4
! ------------
       subroutine bilinin(UF,VF,x0f,dxxf,dyxf,y0f,dxyf,dyyf &
                           ,imaxf,jmaxf,                    &
                          UT,VT,xt,yt)
!c                 ========
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C Interpolates from a regular full field into xt,yt
!C regular full field
!C Input geometry:
!C   x= x0f + I dxxf + J dyxf
!C   y= y0f + I dxyf + J dyyf
!C
!C
!C
!C Interpolated field Tt computed from field Tf
!C
!C
!C JMB 15/5/93
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      integer imaxf,jmaxf,imaxt,jmaxt,i,j,ii,jj
      real*4 UF(imaxf,jmaxf)
      real*4 VF(imaxf,jmaxf)
      real*4 UT,VT
      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf
      real*4 ri,rj,det,xt,yt,xi,yi,xj,yj
      real*4 xbt,ybt
!c     write(6,*) 'Interpolating',x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf
!c     write(6,*) ' to',x0t,dxxt,dyxt,y0t,dxyt,dyyt,imaxt,jmaxt
!c
      det=dxxf*dyyf-dxyf*dyxf
      XI=dyyf/det
      YI=-dyxf/det
      XJ=-dxyf/det
      YJ=dxxf/det

       Ri = XI * ( xt - x0f ) + YI * ( yt - y0f )
       Rj = XJ * ( xt - x0f ) + YJ * ( yt - y0f )
       ii = Ri
       jj = Rj
       Ri = Ri - ii
       Rj = Rj - jj

       if( ii.lt.1 ) then
        if(jj.lt.1 ) jj=1
        if(jj.ge.jmaxf) jj=jmaxf
        UT=UF(1,jj)
        VT=VF(1,jj)
                     else
          if(ii.ge.imaxf) then
             if(jj.lt.1 ) jj=1
             if(jj.ge.jmaxf) jj=jmaxf
             UT=UF(imaxf,jj)
             VT=VF(imaxf,jj)
                          else
             if(jj.lt.1 ) then          
                 UT=UF(ii,1)
                 VT=VF(ii,1)
                          else
                 if(jj.ge.jmaxf) then          
                  UT=UF(ii,jmaxf)
                  VT=VF(ii,jmaxf)
                                  else
!C Interpolate...
                  UT= rj* ( ri * UF(ii+1,jj+1)                &
                                + ( 1 - ri )*UF(ii,jj+1) )    &
                 + (1 -rj) * ( ri * UF(ii+1,jj)               &
                                + ( 1 - ri )*UF(ii,jj) )
                  VT= rj* ( ri * VF(ii+1,jj+1)                &
                                + ( 1 - ri )*VF(ii,jj+1) )    &
                 + (1 -rj) * ( ri * VF(ii+1,jj)               &
                                + ( 1 - ri )*VF(ii,jj) )

                      endif
             endif
          endif
        endif
 99     continue
        return
        end subroutine

        
! Procedure 5
! -----------
        
subroutine bilininl(UF,x0f,dxxf,dyxf,y0f,dxyf,dyyf     &
                           ,imaxf,jmaxf,                              &
                          UT,xt,yt)
!c                 ========
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C Interpolates from a regular full field into xt,yt
!C regular full field
!C Input geometry:
!C   x= x0f + I dxxf + J dyxf
!C   y= y0f + I dxyf + J dyyf
!C
!C
!C
!C Interpolated field Tt computed from field Tf
!C
!C
!C JMB 15/5/93
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      integer imaxf,jmaxf,imaxt,jmaxt,i,j,ii,jj
      real*4 UF(imaxf,jmaxf)

      real*4 UT,VT
      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf
      real*4 ri,rj,det,xt,yt,xi,yi,xj,yj
      real*4 xbt,ybt
!c     write(6,*) 'Interpolating',x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf
!c     write(6,*) ' to',x0t,dxxt,dyxt,y0t,dxyt,dyyt,imaxt,jmaxt
!c
      det=dxxf*dyyf-dxyf*dyxf
      XI=dyyf/det
      YI=-dyxf/det
      XJ=-dxyf/det
      YJ=dxxf/det

       Ri = XI * ( xt - x0f ) + YI * ( yt - y0f )
       Rj = XJ * ( xt - x0f ) + YJ * ( yt - y0f )
       ii = Ri
       jj = Rj
       Ri = Ri - ii
       Rj = Rj - jj

       if( ii.lt.1 ) then
        if(jj.lt.1 ) jj=1
        if(jj.ge.jmaxf) jj=jmaxf
        UT=UF(1,jj)

                     else
          if(ii.ge.imaxf) then
             if(jj.lt.1 ) jj=1
             if(jj.ge.jmaxf) jj=jmaxf
             UT=UF(imaxf,jj)

                          else
             if(jj.lt.1 ) then          
                 UT=UF(ii,1)

                          else
                 if(jj.ge.jmaxf) then          
                  UT=UF(ii,jmaxf)

                                  else
!C Interpolate...
                  UT= rj* ( ri * UF(ii+1,jj+1)                   &
                                + ( 1 - ri )*UF(ii,jj+1) )       &
                 + (1 -rj) * ( ri * UF(ii+1,jj)                  &
                                + ( 1 - ri )*UF(ii,jj) )


                      endif
             endif
          endif
        endif
 99     continue
        return
        end subroutine

! Procedure 6
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  CONSTR (MODULE)
!C     -  RDPROP (READ THE NODAL PROPERTIES FOR CONSTRAINT IMPLEMENTATION
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             CONSTR MODULE                            C
!C          Input of information for constraint implementation          C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine constr(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  READ GENERAL DATA
!C
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
         read(49,*) theta,visc
       if (theta.eq.0.) then
        write(6,*) ' Warning Constraint weight (theta)=',theta
        write(6,*) ' No advection constraint',theta
        itcs=0 
        return
       endif
       
      
         if(ipr.gt.0) write(6,*) ' Constraint weight (theta) =',theta
         if(ipr.gt.0) write(6,*) ' Constraint viscosity =',visc
         if(ipr.gt.0) write(6,*) ' Number of nodal properties =',nnpr
       if(icoord.eq.1) then
        write(6,*) 'Coordinate change is active'
        write(6,*) 'Viscosity and velocity adapted to km scales'
!C what counts is the reynolds number.
        visc=visc/1000
       endif
       
       endif
       
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> TPROP(I,J)    : VALUE OF NODAL PROPERTY J at NODE I
!C

       call allody(nnpr*nnt1,1,'tprop',ltprop,ipr)
       call rdprop(s(ltprop),s(ltcoog),ipr)
       return
      
      end subroutine

! Procedure 7
! -----------

      subroutine rdprop(tprop,tcoog,ipr)
!C
!C  I/O NODAL PROPERTIES FOR CONSTRAINT IMPLEMENTATION
!C
      include'divapre.h'
      include'divainc.h'
      real*4 valexll,valexuu
      dimension tprop(nnt1,nnpr),tcoog(nnt1,2)
      if (itcs.eq.1.or.itcs.eq.3) then
      write(6,*) 'Advection constrained activated'
!CJMB Initialize
      call JMBCONSTR(tprop(1,1),tprop(1,2),tcoog(1,1),tcoog(1,2),0,  icoordchange,valexuu)
!C,dxkm,dykm)
      do 10 i=1,nnt1
        call JMBCONSTR(tprop(i,1),tprop(i,2),tcoog(i,1),tcoog(i,2),1,  icoordchange,valexuu)
!C,dxkm,dykm)
!C         read(50,*) (tprop(i,j),j=1,nnpr)
!c         if (tprop(i,1).ge.99.0.or.tprop(i,2).ge.99.0)then
!c           tprop(i,1)=0.
!c           tprop(i,2)=0.
!c
!c         endif
 10   continue
!C
!C When Finished, deallocate working array
      call allody((-1)*NSWSP,1,'WORKS',1*IPWSP,1)
!C  OUTPUT OF B.C. DESCRIPTION
!C
      if(ipr.ge.3) then
         write(6,*)' List of  nodes  and  NODAL PROPERTIES             '
         write(6,*)' --------------------------------------------------'
         do 100 i=1,nnt1
           write(6,*) i,(tprop(i,j),j=1,nnpr)
 100     continue
      endif

!C      call propllxy(tprop,ipr)
!CJMB changed to mean square
      umean=0.
      do 20 i=1,nnt1
          umean=umean+(tprop(i,1)*tprop(i,1)+tprop(i,2)*tprop(i,2))
 20   continue

!C      write(6,*) 'umean ', umean
!C JMB 2007 replaced alpha1 by 2/L^2 so that xi=0 remains possible
!C
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
      
!C VARIABLE L?
      if(itcs.eq.2.or.itcs.eq.3) then
      write(6,*) 'Variable L activated'
      if (itcs.eq.2) then
       JJPOS=1
                     else
       JJPOS=3
      endif
      
      
            write(6,*) 'Variable L activated'
!CJMB Initialize
      call JMBCONSTRL(tprop(1,JJPOS),tcoog(1,1),tcoog(1,2),0,     icoordchange,valexll)
!C,dxkm,dykm)
      do 1010 i=1,nnt1
        call JMBCONSTRL(tprop(i,JJPOS),tcoog(i,1),tcoog(i,2),1,   icoordchange,valexll)
!C,dxkm,dykm)
!C         read(50,*) (tprop(i,j),j=1,nnpr)
         if (tprop(i,JJPOS).ge.99.0)then
           tprop(i,JJPOS)=1.
         endif
         if (tprop(i,JJPOS).le.0.0)then
           tprop(i,JJPOS)=1.
         endif

 1010   continue
!C
!C When Finished, deallocate working array
      call allody((-1)*NSWSP,1,'WORKS',1*IPWSP,1)
!C  OUTPUT OF B.C. DESCRIPTION
      
      
      
      
      endif
!C End variable L
      return
      end subroutine

! Procedure 8
! -----------
      
      subroutine JMBCONSTR(U,V,X,Y,II,icord,valexuu)
!C,dxkm,dykm)
      include 'divapre.h'
      include 'divainc.h'
      real*8 U,V,X,Y
      

      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf,xt,yt,UT,VT

      integer II,imaxf,jmaxf,icord
      integer imaxff,jmaxff
!c      parameter(imaxff=500,jmaxff=500)
!c      real*4 UF(imaxff,jmaxff),VF(imaxff,jmaxff)
      real*8 xx,yy,dxkm,dykm
      real*4 valexuu
      COMMON/JMBCON/x0f,dxxf,dyxf,y0f,dxyf,dyyf,UF,VF,imaxf,jmaxf,ierruv
      U=0
      V=0
      
      if(II.EQ.0) then
      ierruv=0
!C implement matrix reading
!C Use units 91 92 (U,V) and 93(grid info)
      read(93,*,end=922,err=922) x0f
      read(93,*,end=922,err=922) y0f
      read(93,*,end=922,err=922) dxxf
      read(93,*,end=922,err=922) dyyf
      read(93,*,end=922,err=922) imaxf
      read(93,*,end=922,err=922) jmaxf
      
      x0f=x0f-dxxf
      y0f=y0f-dyyf
!C
!c      if(imaxf*jmaxf.gt.(imaxff*jmaxff)) then
!c      write(6,*) 'increase imaxff*jmaxff to', imaxf*jmaxf
!c      goto 922
!c      endif
!C Use allody and save pointer (to use allody with negative values afterwards?)
!C
      NSWSP=2*imaxf*jmaxf
!C Try to read in simple precision in two fields (eq 1 double precision)
      NSWSP=imaxf*jmaxf+mod(imaxf*jmaxf,2)
      call allody(1*NSWSP,1,'WORKS',1*IPWSP,1)
      vrepl=0
      call uur(91,S(IPWSP),imaxf,jmaxf,valexuu,0)
!c      write(6,*) 'NSWSP',NSWSP,NSWSP/2
      call uur(92,S(IPWSP+NSWSP/2,imaxf,jmaxf,valexuu,0)
!C      call uur(91,UF,imaxf,jmaxf)
!C      call uur(92,VF,imaxf,jmaxf)
      goto 923
 922  continue
      write(6,*) 'Error reading grid info for velocity constraint'
      U=0
      V=0
      ierruv=1
      return
 
!C replace exclusion values by zero
!C Need to change coordinates for x0 etc...

      write(6,*) 'Preparing constraint'
      write(6,*) 'Coordinate change',icord
!CJMB TEST
 
 923  continue
!C End TEST UF VF dyyf dxxf x0f y0f imaxff jmaxff = read in
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
      


      return
      endif
      if(II.EQ.1) then

      if(ierruv.eq.1) then
      U=0
      V=0
      return
      endif
      xt=X
      yt=Y
       
       call bilinin(S(IPWSP),S(IPWSP+NSWSP/2),  &
       x0f,dxxf,dyxf,y0f,dxyf,dyyf              &
                           ,imaxf,jmaxf,        &
                          UT,VT,xt,yt)
      U=UT
      if(icoordchange.lt.0) U=U*dxkm
      V=VT
!C      write(6,*) 'X,Y',x,y,UT,VT
!C This assumes we use velocities (in m/s). So of coordinates in degrees
!C normally coordinate change is required on x,y but not on u,v
!C Should U,V be rather gradients, they must be gradients in the km space, since the are not modified.
!C In other words, U,V should not be derivatives with respect to long/ lat.
      return
      endif
      end subroutine

! Procedure 9
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  COORD (MODULE)
!C     -  TOPOLOLLXY (coordinate change ll to xy for topology)
!C     -  TOPOLOXYLL (coordinate change xy to ll for topology)
!C     -  DATALLXY   (coordinate change ll to xy for data)
!C     -  DATAXYLL   (coordinate change xy to ll for data)
!C     -  XYLL       (coordinate change xy to ll)
!C     -  LLXY       (coordinate change ll to xy)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             COORD MODULE                             C
!C          Coordinate change ll to xy and xy to ll if requested        C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine coord(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  INPUT OF GENERAL DATA
!C
      isspheric=0
      read(10,*) rcoordchange
      if (rcoordchange.gt.0) icoordchange=1
      if (rcoordchange.gt.1.5) then
      write(6,*) 'Pseudo-spherical'
      isspheric=1
      endif
      if (rcoordchange.lt.0) icoordchange=-1
      if(ipr.gt.0) write(6,*) ' Coordinate change', icoordchange 

      return
      end subroutine

! Procedure 10
! -----------

      subroutine llxy(x,y)
      include'divapre.h'
      include'divainc.h'

!C      write(6,*) 'BEF x,y',x,y
      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180))
      x=(x-rlonmean)*xxx
      else
      x=(x-rlonmean)*dxkm
      y=(y-rlatmean)*dykm
      endif
!C      write(6,*) 'AFT x,y',x,y

      return
      end subroutine

! Procedure 11
! -----------

      subroutine xyll(x,y)
      include'divapre.h'
      include'divainc.h'
      if(isspheric.eq.1)  then
      dyyy=(rlatmax-rlatmin)/1000
      xxx=max(cos(y*RPI/180.),cos(RPI/2-dyyy*RPI/180))
      x=x/xxx+rlonmean
      else
      x=x/dxkm+rlonmean
      y=y/dykm+rlatmean
      endif
      return
      end subroutine

! Procedure 12
! -----------

!C
!C CHANGE SCALE OF ALPHA0 and ALPHA1 if COORDINATE CHANGE
!C

      subroutine alphallxy()

      include'divapre.h'
      include'divainc.h'

!C      write(6,*) 'alphamr',rl0,alpha0, alpha1,dykm
      rl0=sqrt(2./alpha1)
      rl0=rl0*dykm
      if (alpha0.ne.0) alpha0=1/(rl0**4)
      alpha1=2/(rl0**2)

!C      write(6,*) 'alphamr',rl0,alpha0, alpha1,dykm
      return
      end subroutine

! Procedure 13
! -----------

      subroutine topollxy(tcoog,ipr)

!C
!C COORDINATE CHANGE TOPO LL to xy
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2)
!C
!C FIND LAT MIN, LAT MAX
      if (icoordchange.lt.0) then
      write(6,*) 'Anisotropic case'
      rlonmin=tcoog(1,1)
      rlonmax=tcoog(1,1)
      rlatmin=tcoog(1,2)
      rlatmax=tcoog(1,2)
      do 1011 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
 1011   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=1
      dxkm=-rcoordchange
      
      endif
      
      
      if (icoordchange.eq.1) then
      rlonmin=360.
      rlonmax=-360.
      rlatmin=90.
      rlatmax=-90.
      do 10 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
!C         write(6,*) tcoog(i,1),tcoog(i,2)
 10   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=(rlatmin+rlatmax)/2.
      dykm=(4*asin(1.)*6360.)/360.
      dxkm=asin(1.)*rlatmean/90.
      dxkm=6360.*cos(dxkm)
      dxkm=(4*asin(1.)*dxkm)/360.

      if(ipr.ge.3) then
         write(6,*)' nnt1:',nnt1 
         write(6,*)' rlonmin:',rlonmin 
         write(6,*)' rlatmin:',rlatmin 
         write(6,*)' rlatmax:',rlatmax 
         write(6,*)' rlatmean:',rlatmean 
         write(6,*)' dxkm:',dxkm 
         write(6,*)' dykm:',dykm 
      endif
      endif
      
      
      if (isspheric.eq.1) then
      write(6,*) 'Spherical case'
      rlonmin=tcoog(1,1)
      rlonmax=tcoog(1,1)
      rlatmin=tcoog(1,2)
      rlatmax=tcoog(1,2)
      do 1091 i=1,nnt1
         rlonmin=min(rlonmin,tcoog(i,1))
         rlonmax=max(rlonmax,tcoog(i,1))
         rlatmin=min(rlatmin,tcoog(i,2))
         rlatmax=max(rlatmax,tcoog(i,2))
 1091   continue

      rlonmean=(rlonmin+rlonmax)/2.
      rlatmean=0
      dykm=1
      dxkm=1
      write(6,*) 'Mean longitude',rlonmean
      endif
      
      
      
      do 20 i=1,nnt1
       call llxy(tcoog(i,1),tcoog(i,2))
 20   continue
!C
!C OUTPUT OF DATA SET DESCRIPTION
!C
      if(ipr.ge.3) then
         write(6,*)' List of X and Y pos (llxy) of topology'
         write(6,*)' --------------------------------------'
         do 100 i=1,nnt1
           write(6,*) tcoog(i,1),tcoog(i,2)
 100     continue
      endif

      call alphallxy()

      return
      end subroutine

! Procedure 13
! -----------


      subroutine datallxy(tdata,ipr)
 
!C
!C COORDINATE CHANGE LL to XY
!C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

      do 20 i=1,ndata
       call llxy(tdata(i,1),tdata(i,2))
       tdata(i,4)=tdata(i,4)/(dykm**2)
 20   continue
!C
!C OUTPUT OF DATA SET DESCRIPTION
!C
      if(ipr.ge.3) then
       write(6,*)' List of X and Y pos (llxy), value and weight of data'
       write(6,*)' ----------------------------------------------------'
       do 100 i=1,ndata
         write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100   continue
      endif
      return
      end subroutine

! Procedure 14
! -----------

      subroutine propllxy(tprop,ipr)
!C
!C IF icoordchange: change aspect ratio of u,v
!C

      include'divapre.h'
      include'divainc.h'

      if (icoordchange.ne.0) then
!c      do i=1,nnt1
!c       tprop(i,1)=tprop(i,1)*dykm/dxkm
!c      enddo


      endif
      return
      end subroutine


! Procedure 15
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             COVAR  MODULE                            C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine covar(ipr)
      include'divapre.h'
      include'divainc.h'
      zero=0.
      read(10,*) ispec
      isumcovar=0
      if(ispec.lt.-100) then
      write(6,*) 'Summing on covariances'
      isumcovar=1
      ispec=ispec+100
      write(6,*) 'ispec',ispec
      endif
      write(6,*) 'Please be patient'
!CJMB TO BE OPTIMIZED: calculate error only if point in mesh; use locopt
!C for regular grid should already be done?? Yes done; so just do it on
!C data points and valatxy with locopt.
      ifkern=0
      JMRELERR=0
      if (ispec.lt.0) then
      ifkern=1
      ispec=-ispec
      if (ispec.gt.10) then
      ispec=ispec-10
      JMRELERR=1
      endif
      write(6,*) 'Full covariance calculation, be patient'
      
      ipipe=0

      if(isumcovar.ne.1) then

            read(59,*) ipipe
      
      if(ipipe.ne.1) then
      close(61)
      open(61,file='../divapipe',form='unformatted')
      else
      
      call pingpong(1,0,1)
!c      call system('sleep 2')
      call pingpong(1,1,1)
      endif
!C only when no summing on covariances
      endif
      
      endif

!C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP -  not validated forthis mathematical problem '
	 stop
      endif
      if(alpha0.eq.zero) then
	 write(6,*) ' STOP - Covariance estimation not valid for alpha0 = 0 '
	 stop
      endif 

      accur=1.e-2
      rl0=sqrt(sqrt(1./alpha0))
      RJMMU=4*3.141592/RL0/RL0
!c      write(6,*) 'rlam',RJMMU
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.
!c      if(abs(atwo).gt.accur) then
!c	 write(6,*) ' STOP - Error estimation not valid for
!c     &               this choice of (alpha0,alpha1) '
!c	 stop
!c      endif


!C  ALLOCATION OF SPACE FOR Covariance infos on location and elements,
!C  MEME HACK Qu'avant, dimensionner avec 1 puis apr�s lecture augmenter
!C  abandonne, plus simple de lire le nombre de donn�es
       read(55,*) ncvdatar
!c      call allody(1,0,'cvpoii',lcvpoii,ipr)
!c      call allody(1,1,'cvpoir',lcvpoir,ipr)
       call allody(2*ncvdatar,0,'cvpoii',lcvpoii,ipr)
       call allody(2*ncvdatar,1,'cvpoir',lcvpoir,ipr)
       

 
      call cvread(l(lcvpoii),s(lcvpoir),ncvdata)
      write(6,*) 'ncvdata?',ncvdata,ncvdatar
!c      call allody(2*ncvdata-1,0,'cvpoii',jjjjj,99)
!c      call allody(2*ncvdata-1,1,'cvpoir',jjjjj,99)
      call allody(nddlt,1,'cvg',lcvg,ipr)
      call allody(ncvdata,1,'lcova',lcova,ipr)
!C      do i=1,ncvdata
!C      write(6,*) 'xy',s(lcvpoir+2*i-2),s(lcvpoir+2*i-1)
!C      write(6,*) 'ei',l(lcvpoii+2*i-2),l(lcvpoii+2*i-1)
!C      enddo
!C
       jjjjco=0
       if(isumcovar.eq.1) then
            doublesum=0
            read(55,*) npxy
            write(6,*) 'npxy',npxy
            call allody(2*npxy,1,'lxycova',lxycova,ipr)
            jjjjco=0
            rewind(79)
            do i=1,npxy
            read(79,*,END=8822,ERR=8822) xxxcov,yyycov
            jjjjco=jjjjco+1
            s(lxycova+2*i-2)=xxxcov
            s(lxycova+2*i-1)=yyycov
            enddo
 8822       continue
            npxy=jjjjco
            write(6,*) 'npxy',npxy
            call allody(npxy,1,'lsumcova',lsumcova,ipr)
            do i=1,npxy
            s(lsumcova+i-1)=0
            enddo
            call allody(ncvdata,1,'lwcova',lwcova,ipr)
!C read here weights
            do i=1,ncvdata
            s(lwcova+i-1)=1
            enddo
            jjjjco=0
       endif
       
       
       
!C if sum to be calculated, loop here over all "data" points; no not needed !
 3344  continue
      jjjjco=jjjjco+1

!c Boucle sur les points de grille ou l'erreur doit etre calculee
      close(80)
      rewind(80)
      open(unit=80,file='fort.80')
      index=0
      jmcount=0
!C Only if ispec=1 3 5 or 7
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 100

      
 10   read(80,*,end=100)xob,yob,iel,isub
      val=0
!C      write(6,*) 'Errors in',xob,yob,iel,isub
      jmcount=jmcount+1
      
      if(mod(jmcount,max(nx*ny/10,1)).eq.0) write(6,*) 'proceeded gridded points:', nint(jmcount*100./(nx*ny)), ' percent'

!C IF Point in mesh
      if(iel.gt.0) then
!C	       write(6,*) 'Point in element',iel,isub,xob,yob
         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         if(ipr.gt.2) write(6,*) 'calgel'
         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)
!C Save right hand side
         do i=1,nddlt
         s(lcvg+i-1)=s(ltrhsg+i-1)
         enddo
         STSB=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         if (ipr.gt.2) write(6,*) 'sol'
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh), nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
	    endif 
         endif
!C Ici boucle sur le reste des points et decision sur fichier fortran de sortie
!C 72, points eux memes, 73 avec les points du fichier 45
!C
      valcv=val
!CJMB BUG??17/7/8 NO, OK
!c      valcv=0
!C JMBBUGEND?
      if(isumcovar.ne.1) then
      if(ipipe.eq.1) then
      open(61,file='../divapipe',form='unformatted')
      rewind(61)
      endif
      
       
      write(61) xob,yob,val/(1+valcv)
      endif
      rjmc0=val/(1+valcv)
!c               write(6,*) '?jmc0a',rjmc0,xob,yob
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
       if (ipipe.eq.1) then
       close(61)
       call pingpong(1,1,0)
!c       write(6,*) 'Now waiting covar'
       call pingpong(1,0,0)
       endif
                         else
      write(6,*) 'Why should I be here?'
      endif
       
       
      endif
!C END if point in mesh

      
      goto 10

 100  continue
!C
!CJMBB now try to put out error field at data points....: DO NOT USE FORT.20
!C USE FORT 45 or directly coordinates already stored... (take care of coordinate change?)
!C Maybe the problem with marina ? Check with and without coordinate change...
!C Only if ispec=2 3 6 or 7
         if((ispec.eq.1).or.(ispec.eq.4).or.(ispec.eq.5)) goto 866
         write(6,*) 'now covariance at data locations'

         
         
         do 1866 iiiii=1,ncvdata
         x=s(lcvpoir+2*iiiii-2)
         y=s(lcvpoir+2*iiiii-1)
         iel=l(lcvpoii+2*iiiii-2)
         isub=l(lcvpoii+2*iiiii-1)
         if(mod(iiiii,max(ncvdata,10)/10).eq.0) write(6,*) 'proceeded', nint(iiiii*100./(ncvdata)), ' percent'

       val=0         
!C now calculate the error in x,y
       xob=x
       yob=y
!C is in mesh?
      if(iel.gt.0) then
	       
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 156        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         valcv=val
!C JMBBUG: NO
!c         valcv=0
!C
         rjmc0=val/(1+valcv)
!c                  write(6,*) '?jmc0b',rjmc0,val,xob,yob
         if(isumcovar.ne.1) then
         if(ipipe.eq.1) then
         open(61,file='../divapipe',form='unformatted')
       rewind(61)
         endif
      write(61) xob,yob,val/(1+valcv)
         endif
      
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
      if (ipipe.eq.1) then
      close(61)
       call pingpong(1,1,0)
       call pingpong(1,0,0)
      endif
                        else
       do i=1,ncvdata
       wwwjco=s(lwcova+iiiii-1)
       wwwico=s(lwcova+i-1)
       doublesum=doublesum+s(lcova+i-1)*wwwjco*wwwico*1./(1.-rjmc0)
       enddo
      
      endif
      
      
      endif
      
!C      goto 666
         

 1866  continue
  866     continue


!CJMBE

!CJMBB now try to put out error field at other discrete locations...
!C only of ispec= 4 5 6 7
         if((ispec.eq.1).or.(ispec.eq.2).or.(ispec.eq.3)) goto 867
         write(6,*) 'Finally covariance at desired discrete locations'
         ireclu=0
         rewind(79)
         jjiijj=0
 667      read(79,*,end=867) x,y
         jjiijj=jjiijj+1
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(mod(ireclu,100).eq.0) write(6,*) 'proceeded',ireclu, ' points'

         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=0
               write(73,*) x_ll,y_ll,valex
               goto 667
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=0
            write(73,*) x_ll,y_ll,valex
            goto 667
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y
!C point in mesh
      if(iel.gt.0) then
	       
         do 157 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
157        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgelcv(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr,xob,yob,iel,isub)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extrt2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extrt3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         valcv=val
!C JMBBUG? NO see paper on efficient calculation
!c         valcv=0
!C END
         rjmc0=val/(1+valcv)
!c         write(6,*) '?jmc0',rjmc0,xob,yob
         if(isumcovar.ne.1) then
         if (ipipe.eq.1) then
         open(61,file='../divapipe',form='unformatted')
       rewind(61)
       endif
      write(61) xob,yob,val/(1+valcv)
         endif
      
      STS=SCAL(s(lcvg),s(lcvg),nddlt)
      SKTS=SCAL(s(ltrhsg),s(lcvg),nddlt)
      STSA=SCAL(s(ltrhsg),s(ltrhsg),nddlt)
!c      write(6,*) 'val,SKTS,STS',valcv,SKTS/RJMMU
      
      do i=1,ncvdata
         xxx=s(lcvpoir+2*i-2)
         yyy=s(lcvpoir+2*i-1)
         iiel=l(lcvpoii+2*i-2)
         iisub=l(lcvpoii+2*i-1)
         if(iiel.le.0) then
         val=0
         else
         if(ityp.eq.2) then
         call extrt2(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	   endif
         if(ityp.eq.3) then
         call extrt3(xxx,yyy,iiel,iisub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	   endif  
         endif
!c         write(6,*) 'in data location', val, valcv,xob,yob,xxx,yyy
!c         write(59,*) val/(1+valcv),xxx,yyy,iiel,iisub
          s(lcova+i-1)=val/(1+valcv)
	enddo
	if(isumcovar.ne.1) then
      write(61) (s(lcova+i-1),i=1,ncvdata)
      if (ipipe.eq.1) then
      close(61)
       call pingpong(1,1,0)
       call pingpong(1,0,0)
      endif
                         else
      do i=1,ncvdata
      wwwiii=s(lwcova+i-1)*1./(1.-rjmc0)
      s(lsumcova+jjiijj-1)=s(lsumcova+jjiijj-1)+wwwiii*s(lcova+i-1)
      enddo
      endif
      
      
      endif
!C end point in mesh
      
      goto 667
         


  867     continue

 1991 continue
 
      if(isumcovar.ne.1) then
      call flush(61)
      if (ipipe.eq.1) then
      write(6,*) 'Last call'
!c      call pingpong(1,1,0)
      write(6,*) 'Getting out'
      endif
      endif
      
      
      if (isumcovar.eq.1) then

!c      if (jjjjco.lt.ncvdata) then
!c      write(6,*) 'next point'
!c      goto 3344
!c                          else
      write(6,*) 'Saving summed covariances',npxy,ncvdata
      
      do i=1,npxy
      write(65,*) s(lsumcova+i-1)
      enddo
      write(64,*) doublesum
!C Save here
!c      endif
      endif
      
      
      
      return
      end subroutine

! Procedure 16
! -----------

      subroutine cvread(lcvelem,cvxy,ncvdata)
      include'divapre.h'
      include'divainc.h'
      dimension cvxy(2,*)
      dimension lcvelem(2,*)
      ncvdata=0   
 10   continue
      read(45,*,end=99) xxx,yyy
      ncvdata=ncvdata+1
      if (icoordchange.ne.0) call llxy(xxx,yyy)
      cvxy(1,ncvdata)=xxx
      cvxy(2,ncvdata)=yyy
!c      write(6,*) 'in cvread',cvxy(1,ncvdata),cvxy(2,ncvdata)
         ipr=1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
            endif
            if (opti.eq.0) then 
               call locpt2(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(xxx,yyy,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(xxx,yyy,tcoog,kconn,tcele,iel,isub,ipr)
            endif


         endif
         lcvelem(1,ncvdata)=iel
         lcvelem(2,ncvdata)=isub
!c         write(6,*) 'in cvread',cvxy(1,ncvdata),cvxy(2,ncvdata)
!c         write(6,*) 'in cvread',lcvelem(1,ncvdata),lcvelem(2,ncvdata)
         goto 10
  99     continue
         write(6,*) ' Covariance with ',ncvdata, ' points'
         return
         end subroutine
      
! Procedure 17
! -----------

      subroutine calgelcv(trhsg,tcoog,trhse,kconn,ipr,xob,yob,iel,isub)
!C
!C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY RHS AND
!C  ASSEMBLING IN THE GLOBAL SECOND MEMBER OF THE SYSTEM
!C
      include'divapre.h'
      include'divainc.h'
      dimension trhsg(nddlt),tcoog(nnt1,2),trhse(nddle),kconn(nelt,nnel)
      one=1.0D0
      three=3.0D0
      five=5.0D0
      
      do  i=1,nddlt
       trhsg(i)=0
      enddo
      do 5 i=1,nddle
	 trhse(i)=0.
 5    continue

!C
!C  OPEN FILE FOR READING CONDENSATION VECTORS (FOR RHS CONDENSATION)
!C      do 10 iel=1,nelt
 

!C JMB Only do it for the element iel and subelement isub of xob, yob ...
        xxx=xob
        yyy=yob
!c        if(ityp.eq.2) then
!c            if (opti.eq.1) then 		! (SvL)
!c               call locpt2opti(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,
!c     &                         l(lkntc),ipr)
!c            endif
!c            if (opti.eq.0) then
!c               call locpt2(xxx,yyy,s(ltcoog),l(lkconn),iel,isub,ipr)
!c            endif
!c         endif
!c         if(ityp.eq.3) then
!c            if (opti.eq.1) then 		! (SvL)
!c            call locpt3opti(xxx,yyy,tcoog,kconn,tcele,iel,isub,kntc,
!c     &                      ipr)
!c            endif
!c            if (opti.eq.0) then
!c            call locpt3(xxx,yyy,tcoog,kconn,tcele,iel,isub,ipr)
!c            endif
!cc
!c
!c         endif
!cC
         if(ityp.eq.2) then
           call cgelecv2 (iel,tcoog,kconn,trhse,l(lklocs),ipr,s(lrkele),xob,yob,isub,s(ltprop))
         endif
         if(ityp.eq.3) then
           call cgelecv3 (iel,tcoog,kconn,trhse,l(lklocs),s(ltcele),ipr,s(lrkele),xob,yob,isub,s(ltprop))
         endif

         IF(IPR.GE.4) then
            WRITE (6,406) iel
         endif
 406     FORMAT(///,T10,' ELEMENTARY  RHS  FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(trhse,nddle,1,nddle,6)
!C
!C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
!C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
!C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
!C
!c         if (iel.eq.500) then
!c         write(6,*) 'into calloc',nddle,lkloce,lkconn
!c         endif
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         ikg=0
         ifg=1
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue

      return
      end subroutine

! Procedure 17
! -----------
      subroutine cgelecv2 (iel,tcoog,kconn,trhse,loces,ipr,trkele,xob,yob,isub,tprop)
!C
!C  INTEGRATE ELEMENTARY RHS WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(12),kconn(nelt,nnel),x(0:3),y(0:3),rll(0:3),TRKELE(3,12,*)
      dimension tprop(nnt1,nnpr)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(15),tgse(10),loces(3,10),tr(3,12)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      zero=0.D0
      do 10 i=1,15
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
!C
      x0=x(0)
      y0=y(0)
      rll0=rll(0)
!C  First sub-element
      if(isub.eq.1) then
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
!c      if(iel.eq.500) then
!c      write(6,*) 'another cgsel2',iel,x0,y0,x1,y1,x2,y2
!c      write(6,*) 'another  k',kconn(iel,1),kconn(iel,3),kconn(iel,5)
!c      write(6,*) '??',tcoog(kconn(iel,1),2),nnt1
!c      endif
      call cgselcv2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
      endif
!C  Second sub-element
!C
      if(isub.eq.2) then
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cgselcv2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
      endif
!C
!C  Third sub-element
!C
      if(isub.eq.3) then
      x1=x(3)
      y1=y(3)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(1)
      endif
      call cgselcv2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
      endif
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.ge.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (15) '
         call impmat(tge,15,1,15,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
!c        IF (IJMB.EQ.0) STOP "KELE OPTI CRASHED"
!C        if(ijmbw(iel).ne.1) stop 'bizzare'
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (15) ==> (12)
!C
      do 160 i=1,12
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+12)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(10)=-trhse(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(11)=-trhse(11)
      endif
      if(kconn(iel,1).lt.kconn(iel,5)) then
            trhse(12)=-trhse(12)
      endif
      return
      end subroutine

! Procedure 18
! -----------
      subroutine cgelecv3(iel,tcoog,kconn,trhse,loces,tcele,ipr,trkele,xob,yob,isub,tprop)
     
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(16),kconn(nelt,nnel),x(0:4),y(0:4),tcele(nelt,2),rll(0:4),TRKELE(3,12,*)
      dimension tprop(nnt1,nnpr)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(19),tgse(10),loces(4,10),tr(3,16)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
      zero=0.d0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(4)=tprop(kconn(iel,7),1)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(4)=tprop(kconn(iel,7),3)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      
      do 10 i=1,19
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 4 (10) SUB-ELEMENT RHS
!C
      x0=x(0)
      y0=y(0)
      rll0=rll(0)
!C
!C  First sub-element
!C
      if(isub.eq.1) then
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
      if(iel.eq.500) then
      write(6,*) 'into cgsel',iel,x0,x0,x1,y1,x2,y2
      endif
      call cgselcv2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
      endif
!C
!C  Second sub-element
!C
      if(isub.eq.2) then
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cgselcv2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
      endif
!C
!C  Third sub-element
!C
      if(isub.eq.3) then
      x1=x(3)
      y1=y(3)
      x2=x(4)
      y2=y(4)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(4)
      endif
      call cgselcv2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
      endif
!C
!C  Fourth sub-element
!C
      if(isub.eq.4) then
      x1=x(4)
      y1=y(4)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(4)
         rll2=rll(1)
      endif
      call cgselcv2(4,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr,xob,yob,rll0,rll1,rll2)
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
 131  continue
      endif
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.gt.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (19) '
         call impmat(tge,19,1,19,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!C        if(ijmbw(iel).eq.0) stop 'etrange'
!c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (19) ==> (16)
!C
      do 160 i=1,16
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+16)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(13)=-trhse(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(14)=-trhse(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
            trhse(15)=-trhse(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
            trhse(16)=-trhse(16)
      endif
      return
      end subroutine

! Procedure 19
! -----------
   subroutine cgselcv2(isub,iel,tgse,x0,y0,x1,y1,x2,y2,kindt,kdata,kelos,tdata,ipr,xob,yob,rll0,rll1,rll2)
!C
!C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tgse(10),kindt(nelt),kdata(ndata),kelos(ndata,2),tdata(ndata,4),tjaci(2,2),tjac(2,2),tr(10,10),gtemp(10),ep(10)
!C       rlrel,rll0,rll1,rll2
      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      do 10 i=1,10
         tgse(i)=zero
         gtemp(i)=zero
	 do 15 j=1,10
            tr(i,j)=zero
 15      continue
 10   continue

!C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
!C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CGSELcv2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
!c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
!c         write(6,*) 'Detj now',detj
        detj=max(abs((x2-x1)*(y0-y1)),abs((x0-x1)*(y2-y1)))
        detj=max(detj,abs((x1-x0)*(y2-y0)),abs((x2-x0)*(y1-y0)))
        detj=max(detj,abs((x1-x2)*(y0-y2)),abs((x0-x2)*(y1-y2)))
        detj=max(detj,abs((x1-x2)*(x1-x2)),abs((x0-x2)*(x0-x2)))
        detj=max(detj,abs((y1-y2)*(y1-y2)),abs((y0-y2)*(y0-y2)))
        detj=max(detj,abs((x1-x0)*(x1-x0)),abs((y0-y1)*(y0-y1)))
        write(6,*) 'changed detj to ',detj
      endif
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      IF(IPR.GE.5) then
         WRITE (6,405) iel,isub
      endif
 405  FORMAT(///,T10,'JACOBIAN MATRIX FOR ELEMENT ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjac,2,2,2,6)
      IF(IPR.GE.5) then
         WRITE (6,406) iel,isub
      endif
 406  FORMAT(///,T10,'INVERSE JAC. MATRIX FOR EL. ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjaci,2,2,2,6)
!C
!C  CONTRIBUTION OF THE DATA POINTS
!C
      rlrel=1
      if(itcs.eq.2.or.itcs.eq.3) then
      rlrel=(rll0+rll1+rll2)/trois
      rlrel=(1.D0/(rll0*rll0)+1.D0/(rll1*rll1)+1.D0/(rll2*rll2))/trois
      rlrel=1.D0/sqrt(rlrel)
      endif

         xo=xob
         yo=yob
         dodo=1
!c         write(6,*) '???covar',iel,RL0
         wo=4*3.141592/RL0/RL0/rlrel/rlrel
         if(ipr.ge.4) then
             write(6,151)xo,yo,dodo,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';yo=',f8.2,';do=',f6.4,';wo=',f10.4)
!C
!C Transformation of the data position in reference element
!C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
!C
!C  CONTRIBUTION FROM PSEUDO-OBSERVATIONS
!C
!CJMB TODO if profiling show too many calculations just above:
!C Store here ep(i) for each data point (idata) and sub-element into big matrix and once stored, skip all the stuff above
!C when contribution at data point is needed
!C right store ep(i,iel,isubelem) should be enought?? Store it from analysis step already
!C
         do 160 i=1,10
!C         ep(i)=TRKELEE(i,idata)
            tgse(i)=tgse(i)+wo*dodo*ep(i)
 160     continue
 150  continue
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
!C
!C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
!C  TR IS THE TRANSFORMATION MATRIX
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 400 k=1,3
         is=(k-1)*3+1
         tr(is,is)=un
         do 410 i=1,2
            do 420 j=1,2
               tr(is+i,is+j)=tjac(i,j)
 420        continue
 410     continue
 400  continue
      tr(10,10)=p1
      tr(10,4)=-trois*p2/(deux*dist12)
      tr(10,7)=-tr(10,4)
      tr(10,5)=p2*(x1-x2)/(quatre*dist12)
      tr(10,8)=tr(10,5)
      tr(10,6)=p2*(y1-y2)/(quatre*dist12)
      tr(10,9)=tr(10,6)
!C
!C TRANSFORMATION OF TGSE
!C JMB TRY TO SAVE TR as for condensation ?? Or is it already this matrix? CHECK if written only
!C only once and used in the same way; if so, try to use it here and not calculate it. Should work
!C since data location invariant during whole process??, not really the same matrix.
!C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
!C         gtemp(i)=gtemp(i)+TRKELEB(k,i,isub,iel)*tgse(k)
         gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue

      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end subroutine

! Procedure 20
! -----------
         subroutine pingpong(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

!c          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           
!c           call system('sleep 0.001')
           read(89,100) istherea
!c           write(6,*) 'w: waiting',istherea
           
           irwtd=irwtd+1
           
           
           
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           
!c           call system('sleep 0.001')
           read(88,100) isthere
!c           write(6,*) 'r: waiting',isthere
           
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
!c          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end subroutine
          
! Procedure 21
! -----------
          
         subroutine pingpongd(IRW,IRWM,IFIRST)
         implicit none
         integer IRW,IRWM,IFIRST,isthere,istherea
         integer irwt,irwtb,irwtc,irwtd
         common /pp/irwt,irwtb,irwtc,irwtd
!C irw=0: reader
!C irw=1: writer
!C irwm=0: read message
!C irwm=1: write message

          write(6,*) 'Getting into pinpong',irw,irwm,ifirst
          if (ifirst.eq.1) then
           if(irw.eq.1) then
           write(6,*) 'Starting writer'
!C ping in write only here
           if(irwm.eq.0) then
           write(6,*) 'w:opening dvping to write'
           open(88,file='../dvping')
           irwt=0
           irwtd=0

!C pong in read only here
           else
           write(6,*) 'w:opening dvpong to read'
           open(89,file='../dvpong')

           endif
                        else
           write(6,*) 'Starting reader'
!C pong in write only here
           if(irwm.eq.1) then
           write(6,*) 'r:opening dvpong to write'
           open(89,file='dvpong')

!C ping in read only here
           else
           write(6,*) 'r:opening dvping to read'
           open(88,file='dvping')
           irwtb=0
           irwtc=0

           endif
           endif
         else
           if(irw.eq.1) then
!C ping in write only here
           if(irwm.eq.1) then
           irwt=irwt+1
           rewind(88)
           write(88,100) irwt
           call flush(88)
           else
!C pong in read only here
 11        continue
           rewind(89)
!c           call system('sleep 0.001')
           read(89,100,end=11,err=11) istherea
!c           write(6,*) 'w: waiting',istherea
           if(istherea.eq.0) goto 11
           irwtd=irwtd+1
           
           rewind(88)
           write(88,100) 0
           
           call flush(88)
           if(istherea.ne.irwtd) then
           write(6,*) '???pingpongw',irwtd,istherea,irwt
           
           endif
           endif
                        else
!C pong in write only here
           if(irwm.eq.1) then
           irwtc=irwtc+1
           rewind(89)
           write(89,100) irwtc
           call flush(89)
!C ping in read only here
           else
           
 111       continue
           rewind(88)
!c           call system('sleep 0.001')
           read(88,100,end=111,err=111) isthere
!c           write(6,*) 'r: waiting',isthere
           if(isthere.eq.0) goto 111
           irwtb=irwtb+1
           if(isthere.ne.irwtb) then
           write(6,*) '???pingpongr',irwtb,isthere,irwtc
           
           endif
           rewind(89)
           write(89,100) 0
           
           call flush(89)
           
           endif
!c           write(6,*) 'received from writer',isthere
           endif
          endif
          write(6,*) 'Getting out of pinpong',irw,irwm,ifirst
!c          call system('sleep 0.001')
 100      format(I10)
          return
          end subroutine

! Procedure 22
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  DATAPR (MODULE)
!C     -  FINDL2 (associate one element to each data to be fitted)
!C     -  FINDL3 (associate one element to each data to be fitted)
!C     -  RDDATA (read the data to be fitted by spline smooting)
!C     -  SORTDT (sort the data according to the sequence of elements)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             DATAPR MODULE                            C
!C             Input of data to be fitted by spline smooting            C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine datapr(ipr)
      include'divapre.h'
      include'divainc.h'
!cJMB another bug??? why should there be the declaration???
!c      dimension kconn(nelt,nnel),tcoog(nnt1,2)
!c jmb

!C  READ IREG :  TYPE OF DATA TREATMENT
!C  IF YOU DON'T WANT TO USE THE OPTIMISATION SYSTEM, JUST WRITE IN
!C  './drv/diva.drv' : IREG+10 INSTEAD OF IREG (SvL)
      read (10,*) ireg 
      if(ireg.lt.10) then
         opti=1
      else 
         opti=0
	 ireg=ireg-10
      endif

!c     opti=0
!C  READ THE DATA SET TO COMPUTE THE NUMBER OF DATA CONSTRAINTS
      index=0
 10   read(20,*,end=100,err=100) xxx
      index=index+1
      goto 10
 100  ndata=index
      rewind(20)
      write(6,*)' Total number of data constraint: ndata =',ndata
      if(ndata.eq.0) then
      write(6,*) 'Will not try to analyse zero data'
      stop
      endif

!C ALLOCATION OF STORAGE TABLES:
!C  ==> TDATA(I,*)  : DATA X and Y POSITION, VALUE and WEIGHT:
!C                    (the weight is defined as the mu factor in
!C                    the P2 Problem:  BRASSEUR, Ph. D. dissert.)
!C  ==> KELOS(ID,*) : LOCALIZATON OF DATA ID IN THE ELEMENT MESH ;
!C                    KELOS(ID,1) = IEL where data is located
!C                    KELOS(ID,2) = SUB-ELEMENT IN IEL (1,2,3 or 4)
!C                    IF IEL<0 ==> DATA NON LOCALIZED
!C  ==> KINDT(IEL)  : INDEX OF THE LAST DATA BELONGING TO (IEL-1) IN
!C                    ARRAY KDATA
!C  ==> KDATA(I)    : DATA NUMBER SEQUENCE, SORTED ELEMENT/ELEMENT
!C  ==> KNTC(nelkntc) : USED FOR OPTIMISATION  (SvL)
!C  ==> KELOS1(nadata) : USED FOR OPTIMISATION FOR THE QUICK SORT ALGORITHM
!C                       (see the 'sordtopti' routine in 'optimi.f')

      ll=4*ndata
      call allody(ll,1,'tdata',ltdata,ipr)
      ll=2*ndata
      call allody(ll,0,'kelos',lkelos,ipr)
      call allody(nelt,0,'kindt',lkindt,ipr)
      call allody(ndata,0,'kdata',lkdata,ipr)
      call rddata(s(ltdata),ipr)
      if (icoordchange.ne.0) then
          call datallxy(s(ltdata),ipr)
      endif

!C COMPUTE THE DIVISIONS OF SPACE FOR OPTIMISATION (SvL)
      if (opti.eq.1) then
         call divesp(s(ltcoog),l(lkconn))
!c JMB added 20
         ncaz=int(100*real(nelt)/real(ncat))+20
!c         write(6,*) 'Ncaz',nelt,ncat,ncaz
         nelkntc=ncax*ncay*ncaz
         call allody(ndata,0,'kelos1',lkelos1,ipr)
!C JMB TO DO
!C Dirty hack first allocate one point (since last allocated can be extended to
!C the end; once repel2 or repel3 finished, allocate what was actually needed
!C but keep old pointer and add nelkntc-1
!C simply put third element * in repel2 and repel3
!C         call allody(nelkntc,0,'kntc',lkntc,ipr)
          nelkntc=ncax*ncay
          call allody(nelkntc,0,'kntc',lkntc,ipr)
!C	       nelkntc=ncax*ncay*ncaz
!C      call allody(nelkntc-1,0,'kntc',jjjjjj,ipr)

      endif

!C ASSOCIATE DATA TO ELEMENTS
!C test
      if(ityp.eq.2) then
      if (opti.eq.1) then 		! SvL
      call repeltest2(s(ltcoog),l(lkconn),l(lkntc),ncamax)
      endif
      endif
      if(ityp.eq.3) then
      if (opti.eq.1) then 		! SvL
      call repeltest3(s(ltcoog),l(lkconn),l(lkntc),ncamax)
      endif
      endif
      if(opti.eq.1) then
!C      write(6,*) 'NCAMAX found',ncamax
!C Si ok, allody ici de ncax*ncay*ncamax EN plus en gardant le pointeur lkntc
          nelkntc=ncax*ncay*ncamax
          ncaz=ncamax+1
!C          write(6,*) 'ncaz',ncaz
          call allody(nelkntc,0,'kntc',jjjjjj,ipr)
      
      endif
      if(ityp.eq.2) then
         if (opti.eq.1) then 		! SvL
            write (6,*) ' *** Optimisation is working 2 ***'
            call repel2(s(ltcoog),l(lkconn),l(lkntc),ncamax)
!c            write(6,*) 'Now found',ncamax
         else 
	    write(6,*) ' *** You asked no optimisation ***'
         endif
         call findl2(l(lkelos),s(ltcoog),l(lkconn),s(ltdata),l(lkntc),ipr)
      endif

      if(ityp.eq.3) then
         if (opti.eq.1) then		! SvL
            write (6,*) ' *** Optimisation is working 3 ***'
            call repel3(s(ltcoog),l(lkconn),l(lkntc),ncamax)
         else 
	    write(6,*) ' *** You asked no optimisation ***'
          endif
         call findl3(l(lkelos),s(ltcoog),l(lkconn),s(ltcele),s(ltdata),ipr)
      endif
!c      write(6,*) 'Space for optimization?',ncamax,ncaz
      
      nelkntc=ncax*ncay*ncaz
!C      call allody(nelkntc-1,0,'kntc',jjjjjj,99)
      
!C --- CODE ADDED FOR 2.2 RELEASE (RS 22 MARCH 94) ---
!c      if (IREG.GT.0.AND.IREG.LT.3) then
          call LINREG (L(LKELOS),S(LTDATA))
!c      endif
!C --- END OF ADDED CODE ---

!C SORT THE DATA
      if (opti.eq.1) then		!SvL
         call sortdtopti(l(lkindt),l(lkdata),l(lkelos),l(lkelos1),ipr)
      endif
      if (opti.eq.0) then
         call sortdt(l(lkindt),l(lkdata),l(lkelos),ipr)
      endif
      write(35,*) ndata,nonloc
      return
      end subroutine

! Procedure 23
! -----------
      subroutine findl2(kelos,tcoog,kconn,tdata,kntc,ipr)

!C  ASSOCIATE ONE ELEMENT TO EACH DATA TO BE FITTED (IF EXISTS)
!C  !!!!!!!!!!!!!!!!  WORKS ONLY FOR ELEMENT OF TYPE 2 !!!!!!!!!!!!!!!!!

      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4),kconn(nelt,nnel),tcoog(nnt1,2),kelos(ndata,2)
      dimension kntc(ncax,ncay,*)

      do 20 id=1,ndata
!c mr
!c        write(6,*) 'Data ', id
         x=tdata(id,1)
         y=tdata(id,2)
         if (opti.eq.1) then 
            call locpt2opti(x,y,tcoog,kconn,iel,isub,kntc,ipr)
!C JMBTEST
!c            if (iel.eq.-1) then
!c            write(6,*) 'sauve qui peut',x,y
!c            call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
!c              if(iel.ne.-1) then
!c              write(6,*) '???problem???',x,y
!c              endif
!c            endif
!C ENDJMBTEST
         endif
         if (opti.eq.0) then 
            call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
         endif
         kelos(id,1)=iel
         kelos(id,2)=isub
 20   continue
      nonloc=0
      do 30 id=1,ndata
         if(kelos(id,1).lt.0) nonloc=nonloc+1
 30   continue
!C
!C  PRINT LOCALIZATION OF DATA IN THE MESH
!C
      if(ipr.ge.1) then
         write(6,910) nonloc
 910     format(/,t2,60('%'),/,' There are ',i7,' data NON localized in the mesh (and ignored)',/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   LOCALIZATION OF DATA IN ELEMENT AND SUB-ELT'
         do 40 id=1,ndata
            write(6,*) id,kelos(id,1),kelos(id,2)
 40      continue
      endif
      return
      end subroutine

! Procedure 24
! -----------
      subroutine findl3(kelos,tcoog,kconn,tcele,tdata,ipr)
!C
!C  ASSOCIATE ONE ELEMENT TO EACH DATA TO BE FITTED (IF EXISTS)
!C  !!!!!!!!!!!!!!!!  WORKS ONLY FOR ELEMENT OF TYPE 3 !!!!!!!!!!!!!!!!!
!C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4),kconn(nelt,nnel),tcoog(nnt1,2),kelos(ndata,2),tcele(nelt,2)
      do 20 id=1,ndata
         x=tdata(id,1)
         y=tdata(id,2)
         if (opti.eq.1) then 
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
         endif
         if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
         endif
         kelos(id,1)=iel
         kelos(id,2)=isub
 20   continue
!C
!C  COUNT THE NUMBER OF NON LOCATED DATA
!C
      nonloc=0
      do 30 id=1,ndata
         if(kelos(id,1).lt.0) nonloc=nonloc+1
 30   continue
!C
!C  PRINT LOCALIZATION OF DATA IN THE MESH
!C
      if(ipr.ge.1) then
         write(6,910) nonloc
 910     format(/,t2,60('%'),/,' There are ',i7,' data NON localized in the mesh (and ignored)',/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   LOCALIZATION OF DATA IN ELEMENT AND SUB-ELT'
         do 40 id=1,ndata
            write(6,*) id,kelos(id,1),kelos(id,2)
 40      continue
      endif
      return
      end subroutine

! Procedure 25
! -----------
      subroutine rddata(tdata,ipr)
!C
!C  I/O DATA SET TO BE FITTED BY SPLINE SMOOTHONG
!C
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
!C
!C  INPUT OF DATA SET DESCRIPTION
!C
!C JMB add calculation of harmonic mean of mu for misfit scaling in GCV
      jmbmud=0
      hmmu=0
      do 10 i=1,ndata
         read(20,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
         if (tdata(i,4).gt.0) then
         jmbmud=jmbmud+1
         hmmu=hmmu+1./tdata(i,4)
         endif
 10   continue
      hmmu=jmbmud/hmmu
!C
!C OUTPUT OF DATA SET DESCRIPTION
!C
      if(ipr.ge.3) then
         write(6,*)' List of X and Y position, value and weight of data'
         write(6,*)' --------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end subroutine

! Procedure 26
! -----------
      subroutine sortdt(kindt,kdata,kelos,ipr)
!C
!C  SORT THE DATA ACCORDING TO THE SEQUENCE OF ELEMENTS
!C
      include'divapre.h'
      include'divainc.h'
      dimension kdata(ndata),kelos(ndata,2),kindt(nelt)
      imaxd=0
      do 10 iel=1,nelt
         kindt(iel)=0
 10   continue

!C  SORTING LOOP (SO SLOW!)
      do 20 idata=1,ndata
         iel=kelos(idata,1)
         if(iel.lt.0) then
            goto 20
         endif
         if(iel.eq.nelt) then
            imaxd=imaxd+1
            kdata(imaxd)=idata
            goto 25
         endif
         imind=kindt(iel+1)+2
         do 40 id=imaxd+1,imind,-1
            kdata(id)=kdata(id-1)
 40      continue
         imaxd=imaxd+1
         kdata(kindt(iel+1)+1)=idata
 25      continue
         do 30 ie=iel+1,nelt
            kindt(ie)=kindt(ie)+1
 30      continue
 20   continue
!C  END OF SORTING LOOP

!C
!C storage of number of data located in the mesh
!C
      ndatl=imaxd
      if (imaxd.le.0) then
       write(6,*) ' Will create valex grid'
       write(43,*) imaxd
      endif
      if(ipr.ge.1) then
         write(6,910) imaxd
 910     format(/,t2,60('%'),/,' There are ',i7,' data localized in the mesh (and resorted)',/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   ORDERED SEQUENCE OF DATA IN ELEMENTS '
         do 50 id=1,imaxd
            write(6,*) id,kdata(id)
 50      continue
         write(6,*)'   VECTOR KINDT   '
         do 60 iel=1,nelt
            write(6,*) iel,kindt(iel)
 60      continue
      endif
      return
      end subroutine

!C --- Compute Linear Regression ---

! Procedure 27
! -----------

      SUBROUTINE LINREG (KELOS,TDATA)

      PARAMETER (NP = 5)

      include'divapre.h'
      include'divainc.h'

      DIMENSION TDATA (NDATA,4), KELOS(NDATA,2)
      REAL*8 XMEAN,TOTDAT,SX,SY,SXY,SX2,SY2,SV,SXV,SYV
      REAL*4 A(NP,NP), B(NP)

      INTEGER*4 INDX(NP)
!C JMB I put D as REAL??
      REAL*4 D
        
!C Compute Mean Value

      IF (IREG.EQ.1) THEN
         XMEAN = 0.
         TOTDAT = 0.
         DO 10 I = 1,NDATA
            IF (KELOS(I,1).GE.0) THEN
               XMEAN = XMEAN + TDATA(I,3)
               TOTDAT = TOTDAT + 1.
            ENDIF
10       CONTINUE 
         IF (TOTDAT.NE.0) XMEAN = XMEAN / TOTDAT

         DO 11 I = 1,NDATA
            TDATA (I,3) = TDATA(I,3) - XMEAN
11       CONTINUE 
         WRITE (22,*) XMEAN
         WRITE (22,*) 'Total Nb  of data : '
         WRITE (22,*) NDATA
         WRITE (22,*) 'Nb of Inside data : '
         WRITE (22,*) TOTDAT
         CLOSE (22)
      ENDIF

!C Compute Linear Regression

      IF (IREG.EQ.2) THEN
         TOTDAT = 0.
         SX  = 0.
         SY  = 0.
         SXY = 0.
         SX2 = 0.
         SY2 = 0.
         SV  = 0.
         SXV = 0.
         SYV = 0.
         DO 20 I = 1,NDATA
            IF (KELOS(I,1).GE.0) THEN
               TOTDAT = TOTDAT + 1.
               SX  = SX  + TDATA(I,1)
               SY  = SY  + TDATA(I,2)
               SXY = SXY + TDATA(I,1)*TDATA(I,2)
               SX2 = SX2 + TDATA(I,1)*TDATA(I,1)
               SY2 = SY2 + TDATA(I,2)*TDATA(I,2)
               SV  = SV  + TDATA(I,3)             
               SXV = SXV + TDATA(I,1)*TDATA(I,3)
               SYV = SYV + TDATA(I,2)*TDATA(I,3)
            ENDIF
 20      CONTINUE

         A(1,1) = TOTDAT
         A(1,2) = SX
         A(1,3) = SY
         A(2,2) = SX2
         A(2,3) = SXY
         A(3,3) = SY2
         A(2,1) = A(1,2)
         A(3,1) = A(1,3)
         A(3,2) = A(2,3)
         B(1) = SV
         B(2) = SXV
         B(3) = SYV

         CALL LUDCMP (A,3,NP,INDX,D)
         IF (D.NE.0) THEN
         CALL LUBKSB (A,3,NP,INDX,B)
                     ELSE
         
         B(1)=B(1)/TOTDAT
         B(2)=0
         B(3)=0
         write(6,*) 'Using average value as reference',B(1)
         ENDIF

         DO 21 I = 1,NDATA
            TDATA (I,3)=TDATA(I,3) -B(1) - B(2) * TDATA(I,1) - B(3) * TDATA(I,2)
21       CONTINUE
         DO 22 I=1,3
            D = D*A(I,I)
22       CONTINUE
         WRITE (22,*) B(1),B(2),B(3)
         WRITE (22,*) 'Total Nb  of data : '
         WRITE (22,*) NDATA
         WRITE (22,*) 'Nb of Inside data : '
         WRITE (22,*) TOTDAT
         WRITE (22,*) 'Determinant of the Matrix :'
         WRITE (22,*) D
         CLOSE (22)
      ENDIF
!c      VARDATA=0
!c      do i=1,ndata
!c      VARDATA=VARDATA+TDATA (I,3)*TDATA(I,3)
!c      enddo
!c      write(6,*) 'Variance of anomalies',VARDATA/ndata
!c      write(33,*) VARDATA/ndata,ndata
      END subroutine
              
! Procedure 28
! -----------

!C -------------------------------------------------
!C --- LUDCMP & LUBKSB :
!C ---                   LU Matrix Decomposition
!C ---                   and Backward Substitution
!C ---
!C --- Numerical Recipies (c)
!C -------------------------------------------------

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) then
        write(6,*) '==========================='
        write(6,*) '======Singular matrix======'
        write(6,*) '==========================='
        AAMAX=1
        D=0
        endif
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
!C JMB???
        imax=N
        write(6,*) 'ludcmp',imax
!C JMBE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      AAMAX=ABS(A(1,1))
      AAMIN=AAMAX
      DO I=1,N
      AAMAX=MAX(AAMAX,ABS(A(I,I)))
      AAMIN=MIN(AAMIN,ABS(A(I,I)))
      ENDDO
      IF(AAMIN.LE.1E-6*AAMAX) then
      write(6,*) 'Probably ill posed fit'
      D=0
      endif
      IF(A(N,N).EQ.0.) then
      A(N,N)=TINY
      D=0
      endif
      write(6,*) 'LUDCMP',AAMIN,AAMAX
      RETURN
      END subroutine

!C ----------------------------------------------
! Procedure 29
! -----------

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END subroutine

! Procedure 30
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  DATAQC (MODULE)
!C     -  CALPSOK : computes pseudo data sets for quality check
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             DATQC  MODULE                            C
!C       Estimates of expected data-analysis differences                C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine dataqc(ipr)
      include'divapre.h'
      include'divainc.h'
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      zero=0.
      AIIAVER=0
      IIAVER=0
      write(6,*) 'Please be patient'

!C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP - Error estimation not validated for this mathematical problem '
	 stop
      endif

      accur=1.e-2
!c      write(6,*) 'divaqc, rl0',rl0
      rl0=sqrt(sqrt(1./alpha0))
!c      write(6,*) 'divaqc, rl0b',rl0
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.

!c      read(10,*)varbak
       rewind(15)
       read(15,*)varbak
       rewind(15)
!c      if(ipr.ge.2) write(6,*)' Variance of bakground =',varbak

 100  continue
!CJMBB now try to put out error field at data points....
         write(6,*) 'now expected analysis-data value at data locations'

         ireclu=0
         rewind(20)
 666      read(20,*,end=866) x,y,tttttt,wwwwww
         x_ll=x
         y_ll=y
         if(mod(ireclu,max(ndata,10)/10).eq.0) write(6,*) 'proceeded',nint(ireclu*100./(ndata)), ' percent'

         
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
               write(76,76) x_ll,y_ll,valex,valex
               goto 666
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
            write(76,76) x_ll,y_ll,valex,valex
            goto 666
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y
       
       if(iel.gt.0) then
            call calpsok(xob,yob,s(ltdata),ipr,ireclu)
!c	 write(6,*) 'exit calpsok',ireclu,ndata,ltrhsg
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 156        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)
!c           write(6,*) 'exit calgel',ireclu,ndata,ltrhsg
!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe
!c         write(6,*) 'try extracting from',iel,isub

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
!c               write(76,*) xob,yob,val,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
!c               write(76,*) xob,yob,val,val
	    endif 
         endif

      endif
!c esterror provides      sqrt(1.D0*varbak(1-val))
      val=1-val*val/varbak
!c      write(6,*) '???',x_ll,y_ll,val
!C VAL contains Aii ??
      errstd=max(1.D0-val,0.D0)
!c      write(6,*) '???',errstd
      rlm=rl0
      if (icoordchange.ne.0) rlm=rlm/dykm
      errstd=sqrt(4*3.1415*varbak/rlm/rlm/wwwwww*errstd)
      AIIAVER=AIIAVER+val
      IIAVER=IIAVER+1
      write(76,76) x_ll,y_ll,errstd,val
      goto 666
         


  866     continue

      write(6,*) "Exact trace average",AIIAVER/IIAVER,IIAVER
!c      write(76,*) AIIAVER/IIAVER,IIAVER

 76   FORMAT(4(E19.7))
      return
      end subroutine


! Procedure 31
! -----------
      subroutine calpsok(xob,yob,tdata,ipr,idatap)

!C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

!C  INPUT OF DATA SET DESCRIPTION
      iiii=0
!c      write(6,*) '???',xob,yob,tdata(idatap,1),tdata(idatap,2)
      do 10 i=1,ndata
!c         call fcorrk(xob,tdata(i,1),yob,tdata(i,2),varbak,rl0,corre)
!c         tdata(i,3)=corre
!c         if(corre.gt.0) iiii=iiii+1
          tdata(i,3)=0
 10   continue
!c      if(iiii.gt.0) then
!c      do 11 i=1,ndata
!c      if(tdata(i,3).gt.0) write(6,*) 'corre',tdata(i,3),iiii
!c         tdata(i,3)=tdata(i,3)/iiii
!c 11   enddo
!c                    else
!c      write(6,*) ' ??? Strange'
!c      endif
       tdata(idatap,3)=varbak
!C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end subroutine

! Procedure 32
! -----------
      subroutine fcorrk(x1,x2,y1,y2,rm0,rl,corre)        
      
      include'divapre.h'
      
      external euclidist                                                
      external bessk1

      r=euclidist(x1,x2,y1,y2)                                        
      eps=r/rl    
                                                                        
      if (eps.le.0.001) then                                                 
         corre=rm0                                                     
      else
         corre=0
      endif                                                             
      return                                                            
      end subroutine
      
! Procedure 33
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  ESTERR (MODULE)
!C     -  CALGEL : calculating and assembling elementary rhs
!C     -  CGELE2 : integrating elementary rhs when ITYP=2
!C     -  CGELE3 : integrating elementary rhs when ITYP=3
!C     -  CGSEL2 : integrating sub-elementary matrices when ITYP=3
!C     -  CALPSO : computes pseudo data sets for error estimates
!C     -  EXTRE2 : extract error estimate when ITYP=2
!C     -  EXTRE3 : extract error estimate when ITYP= 3
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             ESTERR MODULE                            C
!C       Estimates the analysis error (same grid as the analysis)       C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine esterr(ipr)
      include'divapre.h'
      include'divainc.h'
      zero=0.
      read(10,*) ispec
      write(6,*) 'Please be patient'
!CJMB TO BE OPTIMIZED: calculate error only if point in mesh; use locopt
!C for regular grid should already be done?? Yes done; so just do it on
!C data points and valatxy with locopt.
      ifkern=0
      JMRELERR=1
      ipmerror=0
      if (ispec.gt.10) then
      ispec=ispec-10
      ipmerror=1
      igdone=0
      ifkern=-1
      write(6,*) 'Poor man''s error field'
      endif
      if (ispec.lt.0) then
      ifkern=1
      ispec=-ispec
      
      if (ispec.gt.10) then
      ispec=ispec-10
      JMRELERR=0
      endif
      write(6,*) 'Full covariance calculation, be VERY patient'
      ipipe=0
      read(59,*) ipipe
      if(ipipe.ne.1) then
      open(61,file='divapipe',form='unformatted')
      else
      call pingpong(0,0,1)
!c     call system('sleep 1')
       call pingpong(0,1,1)
       iillmm=0
      endif
      endif
      iix=1
      iiy=0
!C Check if the mathematical problem is analog to OA
      if(ipb.ne.2) then
	 write(6,*) ' STOP - Error estimation not validated for this mathematical problem '
	 stop
      endif
      if(alpha0.eq.zero) then
      if (ifkern.ne.1) then
	 write(6,*) ' STOP - Simple error estimation not valid for alpha0 = 0 '
	 stop
	 endif
      endif 

      accur=1.e-2
      rl0=sqrt(sqrt(1./alpha0))
      atwo=rl0*rl0*alpha1
      if(ipr.ge.2) write(6,*)' L0 =',rl0
      if(ipr.ge.2) write(6,*)' atwo =',atwo
      atwo=atwo-2.
      if(abs(atwo).gt.accur) then
       if (ifkern.ne.1.and.ipmerror.ne.1) then
       write(6,*) ' STOP - Simple error estimation not valid for this choice of (alpha0,alpha1) '
	 stop
	endif
      endif 

!c      read(10,*)varbak
      read(15,*)varbak
      if(ipr.ge.2) write(6,*)' Variance of bakground =',varbak

!C Tabulate the bessel function
      if(opti.eq.1) then
         call tabess
      endif

!C  ALLOCATION OF SPACE FOR THE GRIDDED ERROR ESTIMATE
!C

!c Boucle sur les points de grille ou l'erreur doit etre calculee
      close(80)
      rewind(80)
      open(unit=80,file='fort.80')
      index=0
      jmcount=0
!C Only if ispec=1 3 5 or 7
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 100
      call allody(nx*ny,1,'tgrde',ltgrde,ipr)
       do 5 i=1,nx*ny
          s(ltgrde+i-1)=valex
 5     continue

 10   read(80,*,end=100)xob,yob,iel,isub
      val=valex
!C      write(6,*) 'Errors in',xob,yob,iel,isub
      jmcount=jmcount+1
      
      if(mod(jmcount,max(nx*ny/10,1)).eq.0) write(6,*) 'proceeded gridded points: ', nint(jmcount*100./(nx*ny)), ' percent'
      if(iel.gt.0) then
      if(ipmerror.eq.1.and.igdone.eq.1) goto 963
         if(opti.eq.1) then			!	(SvL)
         if(ipr.gt.2) write(6,*) 'calpsoopti'
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif

         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         if(ipr.gt.2) write(6,*) 'calgel'
         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         if (ipr.gt.2) write(6,*) 'sol'
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh), nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe
         if(ipmerror.eq.1) igdone=1
 963     continue

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrde),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif

      endif
!C JMB to replace the regular grid output
      iiy=iiy+1
      if(iiy.gt.ny) then
      iiy=1
      iix=iix+1
      endif
      



!C      ix=nint((xob-xori)/dx)
!C      iy=nint((yob-yori)/dy)
!C      if (isspheric.eq.1) then
!C      xori=xorio
!C      yyy=yob
!C      call llxy(xori,yyy)
!c       write(6,*) 'Spherice',xori,dx,xob,yob
!C      xxx=max(cos(yob*RPI/180),1E-10)
!C      ix=nint((xob-xori)/dx/xxx)
!C      endif
!C      if(iix.ne.ix.or.iiy.ne.iy) then
!C       write(6,*) '????',ix,iix,iy,iiy
!C      endif
      ix=iix
      iy=iiy     
      
      jmboff=(iy-1)*nx+ix-1
!c      write(6,*) 'gridded',ix,iy,jmboff,val
      s(ltgrde+jmboff)=val
!C JMBend??
      goto 10

 100  continue
!C
!CJMBB now try to put out error field at data points....
!C Only if ispec=2 3 6 or 7
         if((ispec.eq.1).or.(ispec.eq.4).or.(ispec.eq.5)) goto 866
         write(6,*) 'now errors at data locations'

         ireclu=0
         rewind(20)
         
 666      read(20,*,end=866) x,y
         x_ll=x
         y_ll=y
         if(mod(ireclu,max(ndata,10)/10).eq.0) write(6,*) 'proceeded',nint(ireclu*100./(ndata)), ' percent'

         
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
               write(72,*) x_ll,y_ll,valex
               goto 666
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
            write(72,*) x_ll,y_ll,valex
            goto 666
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y


      if(iel.gt.0) then
         if(ipmerror.eq.1.and.igdone.eq.1) goto 964
         if(opti.eq.1) then			!	(SvL)
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif
	       
         do 156 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
156        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif
         igdone=1
 964     continue
!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif

      endif
      write(72,*) x_ll,y_ll,val
      goto 666
         


  866     continue


!CJMBE

!CJMBB now try to put out error field at other discrete locations...
!C only of ispec= 4 5 6 7
         if((ispec.eq.1).or.(ispec.eq.2).or.(ispec.eq.3)) goto 867
         write(6,*) 'Finally error at desired discrete locations'
         ireclu=0
         rewind(79)
 667      read(79,*,end=867) x,y
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(mod(ireclu,100).eq.0)  write(6,*) 'proceeded',ireclu, ' points'

         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
               write(73,*) x_ll,y_ll,valex
               goto 667
            endif
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
            write(73,*) x_ll,y_ll,valex
            goto 667
            endif
         endif
         
!C now calculate the error in x,y
       xob=x
       yob=y
      
      if(iel.gt.0) then
      if(ipmerror.eq.1.and.igdone.eq.1) goto 965
         if(opti.eq.1) then			!	(SvL)
            call calpsoopti(xob,yob,s(ltdata),ipr,ifkern)
         else
            call calpso(xob,yob,s(ltdata),ipr)
         endif
	       
         do 157 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
157        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

         igdone=1
965    continue
!c ... extraction de la solution au point observe

         if(ityp.eq.2) then
            call extre2(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif
         if(ityp.eq.3) then
            call extre3(xob,yob,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
	    if(ipr.ge.2) then
!c               write(6,*) 'x,y,error RMS : ',xob,yob,val
               write(85,*) xob,yob,val
	    endif 
         endif

      endif
      write(73,*) x_ll,y_ll,val
      goto 667
         


  867     continue


!CJMBE
      if((ispec.eq.2).or.(ispec.eq.4).or.(ispec.eq.6)) goto 1991
      if(ispec.ge.1) then
         call impmat(s(ltgrde),nx,ny,nx,86)
!c            write(6,*) 'uwritc 87', iprc
           nbm=-1
	 if(iprc.eq.4) then
            call uwritc(87,c8,s(ltgrde),valex,iprc,nx,ny,1,nbm)
!c	    call uwbimg4(87,s(ltgrde),nx,ny,1,valex,dx,dy,xori,yori)
	    write(6,*) 'Storing error estimate in gher format (real*4)'
		       else
            call uwrit2(87,c8,s(ltgrde),valex,iprc,nx,ny,1,nbm)
!c	    call uwbimg8(87,s(ltgrde),nx,ny,1,valex,dx,dy,xori,yori)
	    write(6,*) 'Storing error estimate in gher format (real*4)'
         endif
!c            write(6,*) 'end uwritc 87', iprc
      endif

      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
 1991 continue
      return
      end subroutine

! Procedure 34
! -----------


      subroutine calgel(trhsg,tcoog,trhse,kconn,ipr)
!C
!C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY RHS AND
!C  ASSEMBLING IN THE GLOBAL SECOND MEMBER OF THE SYSTEM
!C
      include'divapre.h'
      include'divainc.h'
      dimension trhsg(nddlt),tcoog(nnt1,2),trhse(nddle),kconn(nelt,nnel)
      one=1.0D0
      three=3.0D0
      five=5.0D0

      do 5 i=1,nddle
	 trhse(i)=0.
 5    continue

!C
!C  OPEN FILE FOR READING CONDENSATION VECTORS (FOR RHS CONDENSATION)
!C
!c      if(ityp.eq.2.or.ityp.eq.3) then
!c         open(32,file='kele2.cnd',recl=nddle*3*iprc,
!c     &         form='unformatted',access='direct')
!c      endif
      do 10 iel=1,nelt
         if(ityp.eq.2) then
           call cgele2 (iel,tcoog,kconn,trhse,l(lklocs),ipr,s(lrkele))
         endif
         if(ityp.eq.3) then
           call cgele3 (iel,tcoog,kconn,trhse,l(lklocs),s(ltcele),ipr,s(lrkele))
         endif

         IF(IPR.GE.4) then
            WRITE (6,406) iel
         endif
 406     FORMAT(///,T10,' ELEMENTARY  RHS  FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(trhse,nddle,1,nddle,6)
!C
!C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
!C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
!C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
!C
!c         if (iel.eq.500) then
!c         write(6,*) 'into calloc',nddle,lkloce,lkconn
!c         endif
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         ikg=0
         ifg=1
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue
      return
      end subroutine

! Procedure 35
! -----------



      subroutine cgele2 (iel,tcoog,kconn,trhse,loces,ipr,trkele)
!C
!C  INTEGRATE ELEMENTARY RHS WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(12),kconn(nelt,nnel),x(0:3),y(0:3),TRKELE(3,12,*)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(15),tgse(10),loces(3,10),tr(3,12)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      zero=0.D0
      do 10 i=1,15
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
!C
      x0=x(0)
      y0=y(0)
!C
!C  First sub-element
!C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
!c      if(iel.eq.500) then
!c      write(6,*) 'another cgsel2',iel,x0,y0,x1,y1,x2,y2
!c      write(6,*) 'another  k',kconn(iel,1),kconn(iel,3),kconn(iel,5)
!c      write(6,*) '??',tcoog(kconn(iel,1),2),nnt1
!c      endif
      call cgsel2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
!C
!C  Second sub-element
!C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      call cgsel2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
!C
!C  Third sub-element
!C
      x1=x(3)
      y1=y(3)
      x2=x(1)
      y2=y(1)
      call cgsel2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.ge.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (15) '
         call impmat(tge,15,1,15,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
!c        IF (IJMB.EQ.0) STOP "KELE OPTI CRASHED"
!C        if(ijmbw(iel).ne.1) stop 'bizzare'
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (15) ==> (12)
!C
      do 160 i=1,12
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+12)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(10)=-trhse(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(11)=-trhse(11)
      endif
      if(kconn(iel,1).lt.kconn(iel,5)) then
            trhse(12)=-trhse(12)
      endif
      return
      end subroutine

! Procedure 36
! -----------


      subroutine cgele3(iel,tcoog,kconn,trhse,loces,tcele,ipr,trkele)
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),trhse(16),kconn(nelt,nnel), x(0:4),y(0:4),tcele(nelt,2),TRKELE(3,12,*)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tge(19),tgse(10),loces(4,10),tr(3,16)
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS)
!C
      zero=0.d0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      do 10 i=1,19
         tge(i)=zero
 10   continue
!C
!C  INTEGRATION OF THE 4 (10) SUB-ELEMENT RHS
!C
      x0=x(0)
      y0=y(0)
!C
!C  First sub-element
!C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(iel.eq.500) then
      write(6,*) 'into cgsel',iel,x0,x0,x1,y1,x2,y2
      endif
      call cgsel2(1,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
 110  continue
!C
!C  Second sub-element
!C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      call cgsel2(2,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
 120  continue
!C
!C  Third sub-element
!C
      x1=x(3)
      y1=y(3)
      x2=x(4)
      y2=y(4)
      call cgsel2(3,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
 130  continue
!C
!C  Fourth sub-element
!C
      x1=x(4)
      y1=y(4)
      x2=x(1)
      y2=y(1)
      call cgsel2(4,iel,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),ipr)
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
 131  continue
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      if(ipr.gt.6) then
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (19) '
         call impmat(tge,19,1,19,6)
	 write(6,*) 'iel = ',iel
      endif
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!C        if(ijmbw(iel).eq.0) stop 'etrange'
!c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY RHS (19) ==> (16)
!C
      do 160 i=1,16
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+16)
 164     continue
         trhse(i)=tge(i)+gimp
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
            trhse(13)=-trhse(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
            trhse(14)=-trhse(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
            trhse(15)=-trhse(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
            trhse(16)=-trhse(16)
      endif
      return
      end subroutine

! Procedure 37
! -----------



      subroutine cgsel2(isub,iel,tgse,x0,y0,x1,y1,x2,y2,kindt,kdata,kelos,tdata,ipr)
!C
!C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tgse(10),kindt(nelt),kdata(ndata),kelos(ndata,2),tdata(ndata,4),tjaci(2,2),tjac(2,2),tr(10,10),gtemp(10),ep(10)
      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      do 10 i=1,10
         tgse(i)=zero
         gtemp(i)=zero
        do 15 j=1,10
            tr(i,j)=zero
 15      continue
 10   continue

!C
!C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
!C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CGSEL2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
!c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
!c         write(6,*) 'Detj now',detj
        
        detj=max(abs((x2-x1)*(y0-y1)),abs((x0-x1)*(y2-y1)))
        detj=max(detj,abs((x1-x0)*(y2-y0)),abs((x2-x0)*(y1-y0)))
        detj=max(detj,abs((x1-x2)*(y0-y2)),abs((x0-x2)*(y1-y2)))
        detj=max(detj,abs((x1-x2)*(x1-x2)),abs((x0-x2)*(x0-x2)))
        detj=max(detj,abs((y1-y2)*(y1-y2)),abs((y0-y2)*(y0-y2)))
        detj=max(detj,abs((x1-x0)*(x1-x0)),abs((y0-y1)*(y0-y1)))
        write(6,*) 'changed detj to ',detj
      endif
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      IF(IPR.GE.5) then
         WRITE (6,405) iel,isub
      endif
 405  FORMAT(///,T10,'JACOBIAN MATRIX FOR ELEMENT ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjac,2,2,2,6)
      IF(IPR.GE.5) then
         WRITE (6,406) iel,isub
      endif
 406  FORMAT(///,T10,'INVERSE JAC. MATRIX FOR EL. ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjaci,2,2,2,6)
!C
!C  CONTRIBUTION OF THE DATA POINTS
!C
      ifirst=kindt(iel)+1
      if(iel.eq.nelt) then
         ilast=ndatl
                      else
         ilast=kindt(iel+1)
      endif
      do 150 id=ifirst,ilast
         idata=kdata(id)
         if(kelos(idata,1).ne.iel) then
            write(6,*)' %%% ERROR - cgsel2 : CHECK DATA SORTING %%% '
            stop
         endif
         isel=kelos(idata,2)
         if(isel.ne.isub) goto 150
         xo=tdata(idata,1)
         yo=tdata(idata,2)
         do=tdata(idata,3)
         wo=tdata(idata,4)
         if(ipr.ge.4) then
             write(6,151)xo,yo,do,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';yo=',f8.2,';do=',f6.4,';wo=',f10.4)
!C
!C Transformation of the data position in reference element
!C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
!C
!C  CONTRIBUTION FROM PSEUDO-OBSERVATIONS
!C
!CJMB TODO if profiling show too many calculations just above:
!C Store here ep(i) for each data point (idata) and sub-element into big matrix and once stored, skip all the stuff above
!C when contribution at data point is needed
!C right store ep(i,iel,isubelem) should be enought?? Store it from analysis step already
!C
         do 160 i=1,10
!C         ep(i)=TRKELEE(i,idata)
            tgse(i)=tgse(i)+wo*do*ep(i)
 160     continue
 150  continue
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
!C
!C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
!C  TR IS THE TRANSFORMATION MATRIX
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 400 k=1,3
         is=(k-1)*3+1
         tr(is,is)=un
         do 410 i=1,2
            do 420 j=1,2
               tr(is+i,is+j)=tjac(i,j)
 420        continue
 410     continue
 400  continue
      tr(10,10)=p1
      tr(10,4)=-trois*p2/(deux*dist12)
      tr(10,7)=-tr(10,4)
      tr(10,5)=p2*(x1-x2)/(quatre*dist12)
      tr(10,8)=tr(10,5)
      tr(10,6)=p2*(y1-y2)/(quatre*dist12)
      tr(10,9)=tr(10,6)
!C
!C TRANSFORMATION OF TGSE
!C JMB TRY TO SAVE TR as for condensation ?? Or is it already this matrix? CHECK if written only
!C only once and used in the same way; if so, try to use it here and not calculate it. Should work
!C since data location invariant during whole process??, not really the same matrix.
!C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
!C         gtemp(i)=gtemp(i)+TRKELEB(k,i,isub,iel)*tgse(k)
         gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue

      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end subroutine

! Procedure 38
! -----------

      subroutine calpso(xob,yob,tdata,ipr)

!C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)

!C  INPUT OF DATA SET DESCRIPTION
      do 10 i=1,ndata
         call fcorr(xob,tdata(i,1),yob,tdata(i,2),varbak,rl0,corre)
         tdata(i,3)=corre
 10   continue
!C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end subroutine

! Procedure 39
! -----------
      subroutine fcorr(x1,x2,y1,y2,rm0,rl,corre)        
      
      include'divapre.h'
      
      external euclidist                                                
      external bessk1

      r=euclidist(x1,x2,y1,y2)                                        
      eps=r/rl    
                                                                        
      if (eps.le.0.001) then                                                 
         corre=rm0                                                     
      else
         corre=rm0*eps*bessk1(eps)                           
      endif                                                             
      return                                                            
      end subroutine                                                              
                                                                       
! Procedure 40
! -----------
      function euclidist(x1,x2,y1,y2) result(value)                                   
                                                                       
      include'divapre.h'
      real*8 value

      value=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))             
      return                                                            
      end function                                                           

! Procedure 41
! -----------
      function bessk1(X) result(value)
      
      include'divapre.h'
      real*8 value

      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,-0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/



      IF(X.LE.0.) STOP 'ERROR X <= 0' 

      IF(X.LE.2.0) THEN
         Y = X * X * 0.25
         value = (LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         Y = 2.0 / X
         value = (EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END function

! Procedure 42
! -----------
      function bessi1(X) result(value)

      include'divapre.h'
      real*8 value

      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,-0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,&
          -0.2895312D-1,0.1787654D-1,-0.420059D-2/

      IF(ABS(X).LT.3.75) THEN
         Y = X*X / (3.75*3.75)
         value = X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
         AX = ABS(X)
         Y = 3.75 / AX
         value = (EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
         IF(X.LT.0.) value = - value
      ENDIF

      RETURN
      END function

! Procedure 43
! -----------

      subroutine extre2(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,val,tgrde,ipr,trkele)
!C
!C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=2
!C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
!C            IS COMPULSORY                               !!!!!!!!!!!
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(3,10), &
               sol(nddlt),tr(3,12),ddl(15),x(0:3),y(0:3),ddlsub(10),     &
!c     &          tjac(2,2),tjaci(2,2),wk(10,10),tgrde(nx,ny),ep(10)     &
               tjac(2,2),tjaci(2,2),wk(10,10),ep(10)                     &
               ,trkele(3,12,*)
     


      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
!C        if(ijmbw(iel).ne.1) stop 'zut'
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
      do 10 i=1,12
        ddl(i)=sol(kloce(i))
 10   continue
!C
!C  CORRECTION FOR NORMAL REFERENCE
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(10)=-ddl(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(11)=-ddl(11)
      endif
!C mr BIG BUG      if(kconn(iel,1).lt.kconn(iel,3)) then
      if(kconn(iel,1).lt.kconn(iel,5)) then
         ddl(12)=-ddl(12)
      endif
      do 20 i=1,3
         ddl(12+i)=zero
         do 25 k=1,12
            ddl(12+i)=ddl(12+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
!C
!C calculate the (XI,ETA) coordinates from (X,Y)
!C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
!C
!C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
!C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if (ipr.ge.3) write(6,*) 'detj ', detj
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
!C
!C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      if (ipr.ge.3) write(6,*) 'dist12 ', dist12
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue
      if (ipr.ge.2) write(6,*) 'val, varbak ', val, varbak
      if (val.gt.varbak) then
       val=0.
      else
       val=sqrt(1.D0*varbak-val)
      endif
!CCCCC
       if(C0C0C0.NE.0) then
       val=sqrt(max(val*val/(1.D0-C0C0C0)-varbak,0.D0))
!C       JMRELERR=1
       if (JMRELERR.EQ.1) then
       val=val*sqrt(1/C0C0C0-1)
       endif
       endif
!CCCCC
      if (ipr.ge.3) write(6,*) 'val, varbak ', val, varbak
!CJMBB
!c      if(ispec.ge.1) then
!c        ix=nint((xp-xori)/dx)
!c        iy=nint((yp-yori)/dy)
!c        tgrde(ix,iy)=val
!c      endif
!CJMBE
      return
      end subroutine

! Procedure 44
! -----------
      subroutine extre3(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,tcele,val,tgrde,ipr,trkele)
!C
!C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=3
!C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
!C            IS COMPULSORY                               !!!!!!!!!!!
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(4,10),&
               sol(nddlt),tr(3,16),ddl(19),x(0:4),y(0:4),ddlsub(10),     &
               tjac(2,2),tjaci(2,2),wk(10,10),tgrde(nx,ny),               &
               tcele(nelt,2),ep(10)                                        &
               ,trkele(3,12,*)

      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
      do 10 i=1,16
        ddl(i)=sol(kloce(i))
 10   continue
!C
!C  CORRECTION FOR NORMAL REFERENCE
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(13)=-ddl(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(14)=-ddl(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
         ddl(15)=-ddl(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
         ddl(16)=-ddl(16)
      endif
      do 20 i=1,3
         ddl(16+i)=zero
         do 25 k=1,16
            ddl(16+i)=ddl(16+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
!C
!C calculate the (XI,ETA) coordinates from (X,Y)
!C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(4)
         y2=y(4)
      endif
      if(isub.eq.4) then
         x1=x(4)
         y1=y(4)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
!C
!C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
!C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
!C
!C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue

      val=sqrt(varbak-val)
!CJMBB
!c      if(ispec.ge.1) then
!c        ix=nint((xp-xori)/dx)
!c        iy=nint((yp-yori)/dy)
!c        tgrde(ix,iy)=val
!c      endif
!C JMBE
      return
      end subroutine

! Procedure 45
! -----------

      subroutine findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)
!C============================================================================
      real*8 x,y,tlcx,tlcy,tmix,tmiy
      integer ncax,ncay,ikn,jkn
!c      include'divapre.h'
!c      include'divainc.h'
!c      dimension kntc(ncax,ncay,*)

!C IN WHICH REGION IS THE DATA?
!cmr      ikn=((x-tmix-mod(x,tlcx))/tlcx)+1
!cmr      jkn=((y-tmiy-mod(y,tlcy))/tlcy)+1
      ikn=((x-tmix-mod(x-tmix,tlcx))/tlcx)+1
      jkn=((y-tmiy-mod(y-tmiy,tlcy))/tlcy)+1
!C JMB test
      ikn=((x-tmix)/tlcx)+1
      jkn=((y-tmiy)/tlcy)+1
!cmr added
      ikn=max(1,min(ikn,ncax))
      jkn=max(1,min(jkn,ncay))
!c      write(6,*) '????',ikn,jkn
!C============================================================================
      return
      end subroutine

! Procedure 46
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  GCVFAC (MODULE)
!C     -  GCVRAN : constructing random data
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             GCVFAC MODULE                            C
!C       Estimates the analysis error (same grid as the analysis)       C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine gcvfac(ipr)
      include'divapre.h'
      include'divainc.h'
      real*8 d0d,d1d,d2d,d3d,d4d,atr,btr,ctr
      real*8 aaagcv,bbbgcv,ggggcv,lll1,lll2,lll3,lll4,ll1,ll2,ll3,ll4
      real*8 llll1,llll2,llll3,llll4
      real*8 gcvaln,vargcv,gcvala,gcvalb,gcval,zval,terr,terra
      real*8 lamnew(12)
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      zero=0.
      gcvala=0
      gcvalb=0
      terr=0
      terra=0
      
      read(10,*) jmbgcv
      ifac=0
      ifirst=1

         do 1009 jjj=1,jmbgcv
         call GCVRAN(s(ltdata),ipr,jjj)
	     zval=0
	     gcval=0
	     terr=0
         do 15 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
15        continue

!
!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         ireclu=0
         rewind(20)
 666      read(20,*,end=866) x,y,tttt,wwww
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
!c         write(6,*) 'gcval.?',ityp,opti
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
!c               write(72,*) x_ll,y_ll,valex
               goto 666
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
!c            write(72,*) x_ll,y_ll,valex
            goto 666
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
         endif
!C only add up points that are located in the mesh
       valb=val
       IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            rewind(22)
            CLOSE (22)
            
            IFIRST = 0
         ENDIF
         VALb = VAL - XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            rewind(22)
            IFIRST = 0
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

         jmboff=ndata*2+ireclu-1
!c         write(6,*) 'data',s(ltdata+jmboff),jmboff,ireclu,ndata
         GCVAL=GCVAL+s(ltdata+jmboff)*(valb)
         terr=terr+valb*valb
         zval=zval+(s(ltdata+jmboff))*(s(ltdata+jmboff))
         goto 666
 866     continue
      gcval=gcval/(zval)
      terr=terr/zval
      gcvala=gcvala+gcval
      terra=terra+terr
 1009 continue
      gcvala=gcvala/jmbgcv
      terra=terra/jmbgcv
      atr=gcvala
      btr=terra
      ctr=atr*btr
      write(6,*) 'Trace average estimate:',gcvala
      write(6,*) 'rms of misfits',GCVALN
      
      
      
      
      
      
      
      
      
!C Factor 1.4 to avoir understmoothing (see???)
      write(77,999) GCVALN/(1-1.0*GCVala),vargcv,GCVALA,GCVALN,(vargcv/gcvaln**2*(1-gcvala)-1),float(NDATL)
 999  format(6(E12.5))
 
 
 
 
 
!C read input from fort.71
!C subtract reference field (make sure coordinates are correct)
!C analyse
!C calculate anomaly of analysis and finally calculate the diagnostics:
!c
!C prepare input data (anomalies)
         call GCVRDT(s(ltdata),ipr,jjj)
!c
         do 415 i=ltrhsg,ltrhsg+nddlt
            s(i)=0.
 415        continue


!C  CALCULATION OF ELEMENTARY SECOND MEMBERS AND INTEGRATION
!C                    IN THE GLOBAL SYSTEM

         call calgel(s(ltrhsg),s(ltcoog),s(ltrhse),l(lkconn),ipr)

!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C  BASED ON THE ALREADY TRIANGULARIZED SYSTEM
         ifac=0
         isol=1
         mp=6
         call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh), nddlt,mp,ifac,isol,isym,energ)
         if(ipr.ge.6) then
            call impsol(s(ltcoog),l(lklink),s(ltrhsg))
         endif

!c ... extraction de la solution au point observe

         ireclu=0
         ijmbval=0
         rewind(20)
 466      read(20,*,end=566) x,y,tttt,wwww
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
!c         write(6,*) 'gcval.?',ityp,opti
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
               val=valex
!c               write(72,*) x_ll,y_ll,valex
               goto 466
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltrhsg),ipr,s(lrkele))
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
!c            write(72,*) x_ll,y_ll,valex
            goto 466
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltrhsg),ipr,s(lrkele))
         endif
!C only add up points that are located in the mesh
       valb=val
       IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            rewind(22)
            CLOSE (22)
            
            IFIRST = 0
         ENDIF
         VALb = VAL - XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            rewind(22)
            IFIRST = 0
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF
         ijmbval=ijmbval+1
         jmboff=ndata*2+ireclu-1

         d2db=d2db+wwww/hmmu*s(ltdata+jmboff)*s(ltdata+jmboff)
         d3d=d3d+wwww/hmmu*s(ltdata+jmboff)*valb
         d4d=d4d+wwww/hmmu*valb*valb

         goto 466
 566     continue
         d2db=d2db/ijmbval
         d3d=d3d/ijmbval
         d4d=d4d/ijmbval
!C now compute the diagnostics for new value of lambda
      RLAMJM=sqrt(1./alpha0)/(4*3.1415)*hmmu
      if(icoordchange.eq.1) RLAMJM=RLAMJM/dykm/dykm
      aaagcv=(d0d-d1d)
      bbbgcv=d0d
      cccgcv=(1-atr)*d0d
      lll1=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll1=max(1D-6,lll1)
      aaagcv=(d0d-2*d1d+d2d)
      bbbgcv=cccgcv
      cccgcv=(1-2*atr+btr)*d0d
      lll2=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll2=max(1D-6,lll2)
      aaagcv=(d0d-3*d1d+3*d2d-d3d)
      bbbgcv=cccgcv
      cccgcv=(1-3*atr+3*btr-ctr)*d0d
      lll3=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll3=max(1D-6,lll3)
      aaagcv=(d0d-4*d1d+6*d2d-4*d3d+d4d)
      bbbgcv=cccgcv
      cccgcv=(1-4*atr+6*btr-4*ctr+ctr*d3d/d2d)*d0d
      cccgcv=(1-4*atr+6*btr-4*ctr+btr*d4d/d2d)*d0d
      lll4=(cccgcv-aaagcv)/(aaagcv*rlamjm-bbbgcv+cccgcv)*rlamjm
      lll4=max(1D-6,lll4)
      ll1= d0d/(d0d-d1d)-1
      ll2= d0d/(d0d-2*d1d+d2d)*(1-atr)-1
      ll3= d0d/(d0d-3*d1d+3*d2d-d3d)*(1-2*atr+btr)-1
      ll4= d0d/(d0d-4*d1d+6*d2d-4*d3d+d4d)*(1-3*atr+3*btr-ctr)-1
      ll1=max(1D-6,ll1)
      ll2=max(1D-6,ll2)
      ll3=max(1D-6,ll3)
      ll4=max(1D-6,ll4)
      if(ll1.lt.rlamjm) then
        llll1=ll1
                        else
                 if(lll1.gt.rlamjm) then
                 llll1=lll1
                 else
                 llll1=(ll1+lll1)/2
                 endif
      endif
      if(ll2.lt.rlamjm) then
        llll2=ll2
                        else
                 if(lll2.gt.rlamjm) then
                 llll2=lll2
                 else
                 llll2=(ll2+lll2)/2
                 endif
      endif
      if(ll3.lt.rlamjm) then
        llll3=ll3
                        else
                 if(lll3.gt.rlamjm) then
                 llll3=lll3
                 else
                 llll3=(ll3+lll3)/2
                 endif
      endif
      if(ll4.lt.rlamjm) then
        llll4=ll4
                        else
                 if(lll4.gt.rlamjm) then
                 llll4=lll4
                 else
                 llll4=(ll4+lll4)/2
                 endif
      endif
      
      lamnew(1)=ll1
      lamnew(2)=ll2
      lamnew(3)=ll3
      lamnew(4)=ll4
      lamnew(5)=lll1
      lamnew(6)=lll2
      lamnew(7)=lll3
      lamnew(8)=lll4
      lamnew(9)=llll1
      lamnew(10)=llll2
      lamnew(11)=llll3
      lamnew(12)=llll4
      call sellam(lamnew,bestguess,guessflag)           
      write(27,*) 'S/N'
      write(27,*) bestguess
      write(27,*) 'Varbak'
      write(27,*) d0d/(1+bestguess)
      write(27,*) 'Quality of guess'
      write(27,*) guessflag
      write(27,*) 'Old value'
      write(27,999)     rlamjm
      write(27,*) 'proposed new values'
      write(27,999)     ll1,ll2,ll3,ll4
      write(27,999)     lll1,lll2,lll3,lll4
      write(27,999)     llll1,llll2,llll3,llll4
 
 
      return
      end subroutine
      
! Procedure 47
! -----------
      
      
      subroutine sellam(lamnew,bestguess,qualflag)
      real*8 lamnew(12),bestguess,qualflag
      
      rm=0
      rvar=0
      do i=1,12
      lamnew(i)=log10(lamnew(i))
      rm=rm+lamnew(i)
      rvar=rvar+lamnew(i)*lamnew(i)
      enddo
      std=sqrt(rvar/12.-(rm/12.)**2)
      
      qualflag=1-std/(rm/12.)
      
      ii=12
 1    continue
      bestguessl=lamnew(ii)
      bestguess=10**lamnew(ii)
      
      if (abs(bestguessl-rm/12.).le.2*std) then
       return
      else
       if (ii.eq.1) then
          bestguess=10**(rm/12.)
          return
       else
         ii=ii-1
         goto 1
       endif
      endif
      return
      end subroutine

! Procedure 48
! -----------

      subroutine GCVRAN(tdata,ipr,iii)

!C RANDOM PSEUDO-DATA USED TO COMPUTE GCV ESTIMATION
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
      integer iseed
      COMMON /CSEED/ ISEED
      iseed=1000000+iii
!C  INPUT OF DATA SET DESCRIPTION
            
      do 10 i=1,ndata/2
          call GRNF(x,y)
          tdata(i,3)=x
          tdata(ndata/2+i,3)=y
!c          write(6,*) 'random',i,x
 10   continue
!c if ndata is odd
          call grnf(x,y)
          tdata(ndata,3)=x
!C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
      return
      end subroutine
      
! Procedure 49
! -----------
      
      SUBROUTINE GRNF(X,Y)
      include'divapre.h'
      integer iseed
      COMMON /CSEED/ ISEED
        PI = 4.0*ATAN(1.0)
        R1 = -LOG(1.0-RANF())
        R2 = 2.0*PI*RANF()
        R1 = SQRT(2.0*R1)
        X  = R1*COS(R2)
        Y  = R1*SIN(R2)
      RETURN
      END subroutine

! Procedure 50
! -----------

      FUNCTION RANF() RESULT(Value)
      include'divapre.h'
      integer iseed,ia,ic,iq,ir
      real*8 Value
      COMMON /CSEED/ ISEED
      DATA IA/16807/,IC/2147483647/,IQ/127773/,IR/2836/
      
        IH = ISEED/IQ
        IL = MOD(ISEED,IQ)
        IT = IA*IL-IR*IH
        IF(IT.GT.0) THEN
          ISEED = IT
        ELSE
          ISEED = IC+IT
        END IF
        Value = ISEED/FLOAT(IC)
      RETURN
      END FUNCTION
      
! Procedure 51
! -----------
      
      subroutine GCVRDT(tdata,ipr,iii)

!C READ ANALYSED DATA AT DATA LOCATION
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
      
      rewind(71)
!C  INPUT OF DATA SET DESCRIPTION
       IFIRST=1
       

            
      do 10 i=1,ndata
          read(71,*) x,y,val
          if (icoordchange.ne.0) call llxy(x,y)
          valb=val
         IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            rewind(22)
            CLOSE (22)
            
            IFIRST = 0
         ENDIF
         VALb = VAL - XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            rewind(22)
            IFIRST = 0
            write(6,*) 'linregval',A0,A1,A2
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

          tdata(i,3)=valb
  10   continue
      return
      end subroutine
      
! Procedure 52
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  MATHPR (MODULE)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             MATHPR MODULE                            C
!C               Description of the mathematical problem                C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine mathpr(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  input of general data
!C
      read(10,*) ityp
      if(ipr.gt.0) write(6,*) ' Finite Element type     =',ityp
      read(10,*) isym
      if(ipr.gt.0) write(6,*) ' Symetric problem : isym =',isym
      read(10,*) ipb
      if(ipr.gt.0) write(6,*) ' Problem type :      ipb =',ipb
!C
!C  PROBLEM P2: (see: BRASSEUR, Ph.D. Dissertation, 1993)
!C
      if(ipb.eq.2) then
         read(12,*) alpha0
         read(12,*) alpha1
         if(ipr.gt.0) write(6,*) ' Parameter alpha0 =',alpha0
         if(ipr.gt.0) write(6,*) ' Parameter alpha1 =',alpha1
      endif
      return
      end subroutine

! Procedure 53
! -----------
      
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  MESHGN (MODULE)
!C     -  COUNTN (count the number of nodes, interfaces .. to be created
!C     -  GENTPO (generate the topology of the finite element mesh;
!C                then, create the topological file on unit 11)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             MESHGN MODULE                            C
!C       Generates a square finite element mesh on a regular grid       C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine meshgn(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  INPUT OF GENERAL DATA
!C
      read(10,*) deltax
      read(10,*) deltay
      read(10,*) nex
      read(10,*) ney
      read(10,*) imatrx
      if(ipr.gt.0) write(6,*) ' X-step of the regular mesh :',deltax
      if(ipr.gt.0) write(6,*) ' Y-step of the regular mesh :',deltay
      if(ipr.gt.0) write(6,*) ' X-number of finite elements :',nex
      if(ipr.gt.0) write(6,*) ' Y-number of finite elements :',ney
      if(ipr.gt.0) write(6,*) ' Read/creat topology matrix ? :',imatrx
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KTPOL   : MATRIX FOR TOPOLOGICAL INFORMATION
!C
      ll=(2+nex)*(2+ney)
      call allody(ll,0,'ktpol',lktpol,ipr)
      call countn(l(lktpol))
      if(ipr.gt.0) write(6,*) ' Total number of vertex nodes :',nnt1
      if(ipr.gt.0) write(6,*) ' Total number of interfaces   :',nnint
      if(ipr.gt.0) write(6,*) ' Total number of nodes        :',nnt
      if(ipr.gt.0) write(6,*) ' Total number of elements     :',nelt
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KLINK(I)    : OPTIMAL POSITION OF NODE (I) IN THE SEQUENCE OF DOF
!C  ==> KSORT(IPOSI): NODE ID IN POSITION (I) IN THE SEQUENCE OF NODES
!C  ==> TCOOG(I,*)  : TABLE OF ABSLUTE COORDINATES OF NODE (I)
!C
      call allody(nnt,0,'klink',lklink,ipr)
      call allody(nnt,0,'ksort',lksort,ipr)
      call allody(2*nnt1,1,'tcoog',ltcoog,ipr)
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KSKYH (I)   :CUMULATED HEIGHT OF STIFFNESS MATRIX COLUMN
!C  ==> KCONN (I,*) :CONNECTIVITY TABLE BETWEEN ELEMENTS AND NODES
!C  ==> KLOCE (I)   :LOCALIZATION OF ONE ELEMENT IN THE STRUCTURE
!C
!C FINITE ELEMENT ITYP=2 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.2) then
         nnel=6
         nddle=12
         nddlt=3*nnt1+nnint
      endif
!C
!C FINITE ELEMENT ITYP=3 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.3) then
         nnel=8
         nddle=16
         nddlt=3*nnt1+nnint
!C SPACE ALLOCATION FOR CENTER OF ELEMENT (CALCULTED ONCE)
         call allody(2*nelt,1,'tcele',ltcele,ipr)
      endif
!C
!C WHATEVER THE TYPE OF ELEMENT ...
!C
      if(ipr.gt.0) write(6,*) ' Total number of deg. of frd. :',nddlt
      call allody(nddlt+1,0,'kskyh',lkskyh,ipr)
      call allody(nnel*nelt,0,'kconn',lkconn,ipr)
      call gentpo(s(ltcoog),l(lkconn),l(lklink),l(lksort),s(ltcele),l(lktpol),ipr)
      call allody(nddle,0,'kloce',lkloce,ipr)
      call calsky(l(lkskyh),l(lkconn),l(lklink),l(lkloce),ipr)
      return
      end subroutine

! Procedure 54
! -----------


      subroutine countn(ktpol)
      include'divapre.h'
      include'divainc.h'
      dimension ktpol(0:nex+1,0:ney+1)
!C
!C  READ OR CREATE THE TOPOLOGY MATRIX KTPOL:
!C    ktpol(i,j) = -1   ==> no element has to be created;
!C                  0   ==> an element must be created;
!C                  iel ==> element iel has been created;
!C
      do 20 i=0,nex+1
         ktpol(i,0)=-1
         ktpol(i,ney+1)=-1
 20   continue
      do 25 j=1,ney
         ktpol(0,j)=-1
         ktpol(nex+1,j)=-1
 25   continue
      if(imatrx.eq.0) then
         do 10 i=1,nex
            do 15 j=1,ney
               ktpol(i,j)=0
 15         continue
 10      continue
      endif
      if(imatrx.eq.1) then
         do 30 j=ney,1,-1
            read(14,400)(ktpol(i,j),i=1,nex)
 30      continue
      endif
 400  format(100(i2))
!C
!C  INSPECT EVERY POSSIBILITY
!C
      nelt=0
      nnt1=0
      nnint=0
      do 100 i=1,nex
         do 110 j=1,ney
            if(ktpol(i,j).eq.0) then
            nelt=nelt+1
            ktpol(i,j)=nelt
!C               FIRST NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j-1).le.0.and.ktpol(i,j-1).le.0) nnt1=nnt1+1
!C               FIRST INTERFACE
            if(ktpol(i,j-1).le.0) nnint=nnint+1
!C               SECONT NODE
            if(ktpol(i+1,j).le.0.and.ktpol(i+1,j-1).le.0.and.ktpol(i,j-1).le.0) nnt1=nnt1+1
!C               SECOND INTERFACE
            if(ktpol(i+1,j).le.0) nnint=nnint+1
!C               THIRD NODE
            if(ktpol(i+1,j).le.0.and.ktpol(i+1,j+1).le.0.and.ktpol(i,j+1).le.0) nnt1=nnt1+1
!C               THIRD INTERFACE
            if(ktpol(i,j+1).le.0) nnint=nnint+1
!C               FOURTH NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j+1).le.0.and.ktpol(i,j+1).le.0) nnt1=nnt1+1
!C               FOURTH INTERFACE
            if(ktpol(i-1,j).le.0) nnint=nnint+1
            endif
 110     continue
 100  continue
      if(ityp.eq.3) then
         nnt=nnt1+nnint
      endif
      return
      end subroutine

! Procedure 55
! -----------
      subroutine gentpo(tcoog,kconn,klink,ksort,tcele,ktpol,ipr)
!C
!C  I/O OF TOPOLOGIC DATA SET
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),klink(nnt),ksort(nnt),tcele(nelt,2),ktpol(0:nex+1,0:ney+1)
!C
!C  RE-INITIATE THE TOPOLOGY MATRIX
!C
      do 10 i=1,nex
        do 15 j=1,ney
           if(ktpol(i,j).gt.0) ktpol(i,j)=0
 15     continue
 10   continue
      nelt=0
      nns=0
      nni=0
      ipos=0
      do 20 i=1,nex
         do 25 j=1,ney
            if(ktpol(i,j).eq.0) then
            nelt=nelt+1
            tcele(nelt,1)=(i-1)*deltax+deltax*0.5
            tcele(nelt,2)=(j-1)*deltay+deltay*0.5
            ktpol(i,j)=nelt
!C               FIRST NODE
            if(ktpol(i-1,j).le.0.and.ktpol(i-1,j-1).le.0.and.ktpol(i,j-1).le.0) then
               nns=nns+1
               ipos=ipos+1
               ksort(ipos)=nns
               tcoog(nns,1)=deltax*(i-1)
               tcoog(nns,2)=deltay*(j-1)
               kconn(nelt,1)=nns
               goto 112
            endif
            if(ktpol(i-1,j).gt.0) then
               iel=ktpol(i-1,j)
               kconn(nelt,1)=kconn(iel,3)
               goto 112
            endif
            if(ktpol(i-1,j-1).gt.0) then
               iel=ktpol(i-1,j-1)
               kconn(nelt,1)=kconn(iel,5)
               goto 112
            endif
            if(ktpol(i,j-1).gt.0) then
               iel=ktpol(i,j-1)
               kconn(nelt,1)=kconn(iel,7)
               goto 112
            endif
!C               FIRST INTERFACE
 112         if(ktpol(i,j-1).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,2)=-(nni+nnt1)
                goto 113
             endif
             if(ktpol(i,j-1).gt.0) then
                iel=ktpol(i,j-1)
                kconn(nelt,2)=kconn(iel,6)
                goto 113
             endif
!C               SECOND NODE
 113         if(ktpol(i,j-1).le.0.and.ktpol(i+1,j-1).le.0.and.ktpol(i+1,j).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*i
                tcoog(nns,2)=deltay*(j-1)
                kconn(nelt,3)=nns
                goto 114
             endif
             if(ktpol(i,j-1).gt.0) then
                iel=ktpol(i,j-1)
                kconn(nelt,3)=kconn(iel,5)
                goto 114
             endif
             if(ktpol(i+1,j-1).gt.0) then
                iel=ktpol(i+1,j-1)
                kconn(nelt,3)=kconn(iel,7)
                goto 114
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,3)=kconn(iel,1)
                goto 114
             endif
!C               SECOND INTERFACE
 114         if(ktpol(i+1,j).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,4)=-(nnt1+nni)
                goto 115
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,4)=kconn(iel,8)
                goto 115
             endif
!C               THIRD NODE
 115         if(ktpol(i+1,j).le.0.and.ktpol(i+1,j+1).le.0.and.ktpol(i,j+1).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*i
                tcoog(nns,2)=deltay*j
                kconn(nelt,5)=nns
                goto 116
             endif
             if(ktpol(i+1,j).gt.0) then
                iel=ktpol(i+1,j)
                kconn(nelt,5)=kconn(iel,7)
                goto 116
             endif
             if(ktpol(i+1,j+1).gt.0) then
                iel=ktpol(i+1,j+1)
                kconn(nelt,5)=kconn(iel,1)
                goto 116
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,5)=kconn(iel,3)
                goto 116
             endif
!C               THIRD INTERFACE
 116         if(ktpol(i,j+1).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,6)=-(nnt1+nni)
                goto 117
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,6)=kconn(iel,2)
                goto 117
             endif
!C               FOURTH NODE
 117         if(ktpol(i,j+1).le.0.and.ktpol(i-1,j+1).le.0.and.ktpol(i-1,j).le.0) then
                nns=nns+1
                ipos=ipos+1
                ksort(ipos)=nns
                tcoog(nns,1)=deltax*(i-1)
                tcoog(nns,2)=deltay*j
                kconn(nelt,7)=nns
                goto 118
             endif
             if(ktpol(i,j+1).gt.0) then
                iel=ktpol(i,j+1)
                kconn(nelt,7)=kconn(iel,1)
                goto 118
             endif
             if(ktpol(i-1,j+1).gt.0) then
                iel=ktpol(i-1,j+1)
                kconn(nelt,7)=kconn(iel,3)
                goto 118
             endif
             if(ktpol(i-1,j).gt.0) then
                iel=ktpol(i-1,j)
                kconn(nelt,7)=kconn(iel,5)
                goto 118
             endif
!C               FOURTH INTERFACE
 118         if(ktpol(i-1,j).le.0) then
                nni=nni+1
                ipos=ipos+1
                ksort(ipos)=nni+nnt1
                kconn(nelt,8)=-(nnt1+nni)
                goto 119
             endif
             if(ktpol(i-1,j).gt.0) then
                iel=ktpol(i-1,j)
                kconn(nelt,8)=kconn(iel,4)
                goto 119
             endif
 119         continue
             endif
 25      continue
 20   continue

!C
!C  CONSTRUCTION OF KLINK VECTOR; FOR TYPE 2 OR 3: FDV ELEMENT
!C
      if(ityp.eq.2.or.ityp.eq.3) then
         ilink=1
         do 40 iposi=1,nnt
            inod=ksort(iposi)
            klink(inod)=ilink
            ilink=ilink+1
            if(inod.le.nnt1) ilink=ilink+2
 40      continue
      endif
!C
!C OUTPUT OF TOPOLOGICAL DATA / CREATE THE FORT.11 FILE
!C
      if(ipr.ge.3) then
         write(6,*)' List of vertex nodes id, X and Y positions'
         write(6,*)' ------------------------------------------'
      endif
      do 100 i=1,nnt1
         if(ipr.ge.3) then
           write(6,*) i,tcoog(i,1),tcoog(i,2)
         endif
         write(11,*) ksort(i),tcoog(i,1),tcoog(i,2)
 100  continue
      do 105 i=nnt1+1,nnt
         write(11,*) ksort(i)
 105  continue
      if(ipr.ge.3) then
         write(6,*)' List of element id , center and connectivities'
         write(6,*)' ----------------------------------------------'
      endif
      do 195 i=1,nelt
         if(ipr.gt.3) then
           write(11,916) (kconn(i,j),j=1,nnel)
           write(6,915) i,(tcele(i,j),j=1,2),(kconn(i,j),j=1,nnel)
         endif
 915     format(' Elt:',i5,' Center:',2(f7.1),' Nodes:',10(i5))
 916     format(10(i6,' '))
 195  continue
      close(11)
      open(unit=11,file='fort.11')
      if(ipr.ge.4) then
         write(6,*)' I   ,    KSORT(I)    ,     KLINK(I)      '
         write(6,*)' ---------------------------------------- '
         do 120 i=1,nnt
           write(6,*) i,ksort(i),klink(i)
 120     continue
      endif
      return
      end subroutine
                                                                      
! Procedure 56
! -----------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C  The subroutines in this file are used for optimisaton
!C
!C   SUBROUTINE LIST:
!C     -  DIVESP : Subdivises the space for optimisation
!C     -  SIZES2 : Computes the size of the space for elements of type 2
!C     -  SIZES3 : Computes the size of the space for elements of type 3
!C     -  REPEL2 : Distributes the elements of type 2 in kntc table
!C     -  REPEL3 : Distributes the elements of type 2 in kntc table
!C     -  FINDCA : Finds in which region is one point
!C     -  LOCPT2OPTI : locates the (x,y) point in the structure (for ityp=2)
!C     -  LOCPT3OPTI : locates the (x,y) point in the structure (for ityp=3)
!C     -  SORTDTOPTI : sorts the data according to the sequence of elements
!C     -  QS2I1R : Quick Sort algorithm for SORDTOPTI (from www.netlib.org)
!C     -  CALPSOOPTI : computes pseudo data sets for error estimates
!C     -  FCORROPTI : part of calpsoopti
!C     -  TABESS : Tabulates the Bessel function for the calculation of error
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      subroutine divesp(tcoog,kconn)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)

!C COMPUTE THE SIZE OF THE SPACE
      if(ityp.eq.2) then
         call sizes2(s(ltcoog),l(lkconn))
      endif
      if(ityp.eq.3) then
         call sizes3(s(ltcoog),l(lkconn))
      endif

!C COMPUTE CHARACTERISTIC SIZE OF ELEMENTS
      carax=0
      caray=0
!c      neltp=(nelt-mod(nelt,20))/20
      
      neltp=0
      jjstel=nint(nelt/1000.)+1
      iel=1
 300  iel=iel+jjstel
!C JMB addded
      if(iel.gt.nelt) iel=nelt
!C JMB
         carax=carax+(abs(tcoog(kconn(iel,1),1)-tcoog(kconn(iel,3),1))+&
                     abs(tcoog(kconn(iel,1),1)-tcoog(kconn(iel,5),1))+&
                     abs(tcoog(kconn(iel,3),1)-tcoog(kconn(iel,5),1)))&
     /3
         caray=caray+(abs(tcoog(kconn(iel,1),2)-tcoog(kconn(iel,3),2))+  &
                     abs(tcoog(kconn(iel,1),2)-tcoog(kconn(iel,5),2))+  &
                     abs(tcoog(kconn(iel,3),2)-tcoog(kconn(iel,5),2)))  &
     /3
      neltp=neltp+1
      if (iel.lt.nelt-jjstel) then
          goto 300
      endif
      carax=carax/neltp 
      caray=caray/neltp
!c      write(6,*) 'carax',carax,caray
!C COMPUTE THE NUMBERS OF ELEMENTS IN THE KNTC TABLE
      tlex=tmax-tmix      
      tley=tmay-tmiy      
 
      ncax=int(tlex/carax)
      ncay=int(tley/caray)
!Cjmb added 1
      ncax=int(tlex/carax)+1
      ncay=int(tley/caray)+1
      ncat=ncax*ncay
      tlcx=tlex/ncax
      tlcy=tley/ncay
!cmr      write(6,*) ncax,ncay,ncat
!cmr      write(6,*) tlcx,tlcy
!cmr      write(6,*) neltp
!C============================================================================
      return
      end subroutine

! Procedure 57
! -----------

      subroutine sizes2 (tcoog,kconn)
!C============================================================================
!C COMPUTE THE SIZE OF THE SPACE FOR ELEMENTS OF TYPE 2
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      tmax=tcoog(kconn(1,1),1)
      tmix=tcoog(kconn(1,1),1)
      tmay=tcoog(kconn(1,1),2)
      tmiy=tcoog(kconn(1,1),2)
      iel=0
      j=-1

 100  iel=iel+1
 200     j=j+2   
            if (tcoog(kconn(iel,j),1).gt.tmax) then 
               tmax=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),1).lt.tmix) then 
               tmix=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),2).gt.tmay) then 
               tmay=tcoog(kconn(iel,j),2)
            endif
            if (tcoog(kconn(iel,j),2).lt.tmiy) then 
               tmiy=tcoog(kconn(iel,j),2)
            endif
         if (j.lt.5) then
            goto 200 
         endif
      if (iel.lt.nelt) then
         j=-1
         goto 100
      endif
!C============================================================================
      return
      end subroutine

! Procedure 58
! -----------

      subroutine sizes3 (tcoog,kconn)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)

!C COMPUTE THE SIZE OF THE SPACE FOR ELEMENTS OF TYPE 3
      tmax=tcoog(kconn(1,1),1)
      tmix=tcoog(kconn(1,1),1)
      tmay=tcoog(kconn(1,1),2)
      tmiy=tcoog(kconn(1,1),2)
      iel=0
      j=-1

 100  iel=iel+1
 200     j=j+2   
            if (tcoog(kconn(iel,j),1).gt.tmax) then 
               tmax=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),1).lt.tmix) then 
               tmix=tcoog(kconn(iel,j),1)
            endif
            if (tcoog(kconn(iel,j),2).gt.tmay) then 
               tmay=tcoog(kconn(iel,j),2)
            endif
            if (tcoog(kconn(iel,j),2).lt.tmiy) then 
               tmiy=tcoog(kconn(iel,j),2)
            endif
         if (j.lt.7) then
            goto 200 
         endif
      if (iel.lt.nelt) then
         j=-1
         goto 100
      endif
!C============================================================================
      return
      end subroutine

! Procedure 58
! -----------

      subroutine repel2 (tcoog,kconn,kntc,ncamax)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(3),jkntc(3),xn(3),yn(3)

!C MAKING THE KNTC 3D TABLE
!C (1) INITIALISE THE FIRST ELEMENTS OF KNTC
      ncamax=0
!c      write(6,*) 'into repel2'
      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue
!cmr      write(6,*)ncax,ncay,ncaz
!cmr      write(6,*)tmix,tmax,tmiy,tmay
!C (2) TAKE EACH TRIANGLE
      iel=0
 300  iel=iel+1

!C (2.1) TAKE THE THREE NODES OF THIS TRIANGLE
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)

!C (2.2) IN WHICH ELEMENT OF KNTC IS THIS TRIANGLE?
      do 400 i=1,3     
!cmr      ikntc(i)=((xn(i)-tmix-mod(xn(i),tlcx))/tlcx)+1
!cmr      jkntc(i)=((yn(i)-tmiy-mod(yn(i),tlcy))/tlcy)+1
!c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
!c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1

      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=max(1,min(jkntc(i),ncay))
 400  continue     
 
!C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.3) then
         goto 500 
      endif
!cmr      write(6,*) imii,imai,imij,imaj,iel
      if (imii.lt.0.or.&
         imai.lt.0.or.&
         imij.lt.0.or.&
         imaj.lt.0.or.&
         imii.gt.ncax.or.&
         imai.gt.ncax.or.&
         imij.gt.ncay.or.&
         imaj.gt.ncay) then
        write(6,*) 'wrong index', imii,imai,imij,imaj
        write(6,*)  imii,imai,imij,imaj
!cmr        stop
      endif
!C (4)  PUT THE NUMBER OF THE TRIANGLE IN THE FIRST FREE ELEMENTS CORRESPONDING
!C      TO THE BIGGEST RECTANGLE THAT CONTAINS THE TRIANGLE.

      do 800 i=imii,imai
         do 700 j=imij,imaj
            kkntc=0
  600       kkntc=kkntc+1
!cmr               if(kkntc.gt.ncaz) then ! otherwise crashes because kntc(i,j,kkntc+1)=0 fails
               if(kkntc.ge.ncamax) ncamax=kkntc
               if(kkntc.ge.ncaz) then
               write (6,*) 'NCAZ OF TABLE KNTC IS TO SMALL',kkntc,ncaz,iel,i,j,ncamax
               stop
               endif
            if (kntc(i,j,kkntc).eq.0) then
               kntc(i,j,kkntc)=iel
               kntc(i,j,kkntc+1)=0
               goto 700
            endif
            goto 600
 700     continue
 800  continue

      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
!C============================================================================
      return
      end subroutine

! Procedure 59
! -----------

      subroutine repel3 (tcoog,kconn,kntc,ncamax)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(4),jkntc(4),xn(4),yn(4)
      ncamax=0
      write(6,*) 'into repel3'
!C MAKING THE KNTC 3D TABLE
!C (1) INITIALISE THE FIRST ELEMENTS OF KNTC

      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue

!C (2) TAKE EACH ELEMENT
      iel=0
 300  iel=iel+1

!C (2.1) TAKE THE FOUR NODES OF THIS ELEMENT
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)
      xn(4)=tcoog(kconn(iel,7),1)
      yn(4)=tcoog(kconn(iel,7),2)

!C (2.2) IN WHICH ELEMENT OF KNTC IS IT ?
      do 400 i=1,4
!c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
!c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1

      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=min(1,min(jkntc(i),ncay))
 400  continue     
 
!C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.4) then
         goto 500 
      endif

!C  (4) PUT THE NUMBER OF THE ELEMENT IN THE FIRST FREE PLACE CORRESPONDING
!C      TO THE BIGGEST RECTANGLE THAT CONTAINS IT

      do 800 i=imii,imai
         do 700 j=imij,imaj
            kkntc=0
  600       kkntc=kkntc+1
!cmr               if(kkntc.gt.ncaz) then ! otherwise crashes because kntc(i,j,kkntc+1)=0 fails
               if(kkntc.ge.ncamax) ncamax=kkntc
               if(kkntc.ge.ncaz) then
               write (6,*) 'NCAZ OF TABLE KNTC IS TO SMALL',kkntc,ncaz,iel,i,j,nelt
               stop
               endif
            if (kntc(i,j,kkntc).eq.0) then
               kntc(i,j,kkntc)=iel
               kntc(i,j,kkntc+1)=0
               goto 700
            endif
            goto 600
 700     continue
 800  continue
      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
!C============================================================================
      return
      end subroutine

! Procedure 60
! -----------




      subroutine locpt2opti(x,y,tcoog,kconn,ielem,isub,kntc,ipr)
!C============================================================================
!C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 2)

      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      ielem=-1
      isub=-1
      

!C  IN WHICH AREA IS THE (X,Y) POINT ?
      call findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)
!c      write(6,*) 'bound ',ikn,jkn
!C  WHICH ELEMENTS ARE CAPABLE OF CONTAINING DATA ID ?
      kkn=0
    
  100 kkn=kkn+1
      if (kntc(ikn,jkn,kkn).le.0) then
          goto 20
      endif
      iel=kntc(ikn,jkn,kkn)
!c      write(6,*) kkn,iel

!C  DOES DATA ID BELONG TO ELEMENT IEL ?
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
!c      if(iel.eq.500) then
!c      write(6,*) 'loc2opt???',x1,y1,x2,y2,x3,y3
!c      endif
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) then
         goto 100
      endif
!c      write(6,*) 'sub-el?'
!C  WHICH SUB-ELEMENT IN ELEMENT IEL ?
      ielem=iel
      isub=itria
      itria=1
!c      if(itria.eq.0) then
!c         write(6,*) '%%% ERROR  LOCPT2 IN THE LOCALIZATION %%%'
!c         stop
!c      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ', i4,'(',i1,')')
 20   continue
!c      if(ielem.eq.-1) then
!c      write(6,*) 'Locopti problem',x,y,ikn,jkn,tmix,tmiy,tlcx,tlcy
!c      write(6,*) 'kntc table'
!c      kkn=0
!c 9987 continue
!c
!c      write(6,*),kkn+1,kntc(ikn,jkn,kkn+1)
!c      kkn=kkn+1
!c      if (kntc(ikn,jkn,kkn).ne.0) goto 9987
!c      call locpt2(x,y,tcoog,kconn,ielem,isub,ipr)
!c      write(6,*) 'Locpt2 found',ielem,isub
!c      write(6,*) 'of corners'
!c      xx1=tcoog(kconn(ielem,1),1)
!c      yy1=tcoog(kconn(ielem,1),2)
!c      xx2=tcoog(kconn(ielem,3),1)
!c      yy2=tcoog(kconn(ielem,3),2)
!c      xx3=tcoog(kconn(ielem,5),1)
!c      yy3=tcoog(kconn(ielem,5),2)
!c      write(6,*) xx1,yy1,xx2,yy2,xx3,yy3
!c      endif
!C============================================================================
      return
      end subroutine

! Procedure 61
! -----------

      subroutine locpt3opti(x,y,tcoog,kconn,tcele,ielem,isub, &
!C============================================================================
                           kntc,ipr)
!C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 3)
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2),tcele(nelt,2)
      dimension kntc(ncax,ncay,*)
      ielem=-1
      isub=-1

!C  IN WHICH AREA IS THE (X,Y) POINT ?
      call findca (x,y,ikn,jkn,tlcx,tlcy,tmix,tmiy,ncax,ncay)

!C  WHICH ELEMENTS ARE CAPABLE OF CONTAINING DATA ID ?
      kkn=0
    
  100 kkn=kkn+1
      if (kntc(ikn,jkn,kkn).le.0) then
          goto 20
      endif
      iel=kntc(ikn,jkn,kkn)

!C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 1 or 2)?
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      x4=tcoog(kconn(iel,7),1)
      y4=tcoog(kconn(iel,7),2)
      x0=tcele(iel,1)
      y0=tcele(iel,2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 50

!C  WHICH SUB-ELEMENT IN ELEMENT IEL (1 or 2 ) ?
      ielem=iel
      call istria(x,y,x1,y1,x2,y2,x0,y0,itria)
      if(itria.ge.1) then
         isub=1
                     else
         isub=2
      endif
      goto 22
 50   continue

!C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 3 or 4)?
      call istria(x,y,x1,y1,x3,y3,x4,y4,itria)
      if(itria.eq.0) goto 100

!C  WHICH SUB-ELEMENT IN ELEMENT IEL (3 or 4 ) ?
      ielem=iel
      call istria(x,y,x1,y1,x0,y0,x4,y4,itria)
      if(itria.ge.1) then
         isub=4
      else
         isub=3
      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',i4,'(',i1,')')
 20   continue
!C============================================================================
      return
      end subroutine

! Procedure 62
! -----------

      subroutine sortdtopti(kindt,kdata,kelos,kelos1,ipr)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kdata(ndata),kelos(ndata,2),kindt(nelt)
      dimension kelos1(ndata)
   
!C INITIALISE THE ARRAYS
      imaxd=ndata-nonloc
      ndatl=imaxd
      do 10 iel=1,nelt
         kindt(iel)=0
  10  continue
      do 20 idata=1,ndata
         kelos1(idata)=kelos(idata,1)
	 kdata(idata)=idata
  20  continue
!C       write(6,*) '?? QUICK',ndata
!C CALL THE QUICK SORT ROUTINE
      call QS2I1R(kelos1,kdata,ndata)
!c       write(6,*) 'ended'
!C REMOVE THE IGNORED DATAS
      do 30 idata=1,ndata-nonloc
         kdata(idata)=kdata(idata+nonloc)
  30  continue

!C COMPUTE THE KINDT ARRAY
      do 40 idata=1,ndata
         iel=kelos(idata,1)
         if(iel.lt.0) then
            goto 40
         endif
         do 41 ie=iel+1,nelt
            kindt(ie)=kindt(ie)+1
  41     continue
  40  continue

!C storage of number of data located in the mesh
      if (imaxd.le.0) then
       write(6,*) ' Will create valex grid'
       write(43,*) imaxd
      endif
      if(ipr.ge.1) then
         write(6,910) imaxd
 910     format(/,t2,60('%'),/,' There are ',i6,' data localized in the mesh (and resorted)',/,t2,60('%'))
      endif
      if(ipr.ge.4) then
         write(6,*)'   ORDERED SEQUENCE OF DATA IN ELEMENTS '
         do 50 id=1,imaxd
            write(6,*) id,kdata(id)
 50      continue
         write(6,*)'   VECTOR KINDT   '
         do 60 iel=1,nelt
            write(6,*) iel,kindt(iel)
 60      continue
      endif
      return
!C============================================================================
      end subroutine

! Procedure 63
! -----------

      subroutine QS2I1R (IA,JA,N)
!C=============================================================================
!C *** DESCRIPTION (from www.netlib.org)
!C     Written by Rondall E Jones
!C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
!C     algorithm. date 18 November 1976.
!C
!C     Further modified by David K. Kahaner
!C     National Bureau of Standards
!C     August, 1981
!C
!C     Even further modification made to bring the code up to the
!C     Fortran 77 level and make it more readable and to carry
!C     along one integer array and one real array during the sort by
!C     Mark K. Seager
!C     Lawrence Livermore National Laboratory
!C     November, 1987
!C     This routine was adapted from the ISORT routine.
!C
!C     ABSTRACT
!C         This routine sorts an integer array IA and makes the same
!C         interchanges in the integer array JA and the real array A.
!C         The array IA may be sorted in increasing order or decreasing
!C         order.  A slightly modified quicksort algorithm is used.
!C
!C     DESCRIPTION OF PARAMETERS
!C        IA - Integer array of values to be sorted.
!C        JA - Integer array to be carried along.
!C         A - Real array to be carried along.
!C         N - Number of values in integer array IA to be sorted.
!
!C     .. Scalar Arguments ..
      INTEGER N
!C     .. Array Arguments ..
      INTEGER IA(N), JA(N)
!C     .. Local Scalars ..
      REAL R 
      INTEGER I, IIT, IJ, IT, J, JJT, JT, K, KK, L, M, NN
!C     .. Local Arrays ..
      INTEGER IL(21), IU(21)

!C --- FIRST EXECUTABLE STATEMENT  QS2I1R ---
      NN=N
      if (N.EQ.1) then
      write(6,*) 'No need to sort a single data point'
      return
      endif

!C     Sort IA and carry JA and A along.
!C     And now...Just a little black magic...
      M = 1
      I = 1
      J = NN
      R = .375E0
 210  IF( R.LE.0.5898437E0 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875E0
      ENDIF
 225  K = I

!C     Select a central element of the array and save it in location
!C     it, jt, at.
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)

!C     If first element of array is greater than it, interchange with it.
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
      ENDIF
      L=J

!C     If last element of array is less than it, swap with it.
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)

!C     If first element of array is greater than it, swap with it.
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
         ENDIF
      ENDIF

!C     Find an element in the second half of the array which is
!C     smaller than it.
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240

!C     Find an element in the first half of the array which is
!C     greater than it.
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245

!C     Interchange these elements.
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         GOTO 240
      ENDIF

!C     Save upper and lower subscripts of the array yet to be sorted.
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260

!C     Begin again on another portion of the unsorted array.
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      GO TO 265

 300  CONTINUE
      RETURN
!C=============================================================================
      END subroutine

! Procedure 64
! -----------

      subroutine calpsoopti(xob,yob,tdata,ipr,ikfull)
!C=============================================================================
!C  PSEUDO-DATA USED TO COMPUTE ESTIMATION ERROR
      include'divapre.h'
      include'divainc.h'
      dimension tdata(ndata,4)
      
!C JMB FOR FULL KERNEL CALCULATION
      if(ikfull.eq.1) then
      include'kernelbis.inc'
                      else
      if(ikfull.eq.-1) then
       do i=1,ndata
       tdata(i,3)=varbak
       enddo
       write(6,*) 'Poor man''s error uses 1 for correlation'
                       else
!C JME
!C  INPUT OF DATA SET DESCRIPTION
      do 10 i=1,ndata
         call fcorropti(xob,tdata(i,1),yob,tdata(i,2),corre)
         tdata(i,3)=corre
 10   continue
!C OUTPUT OF PSEUDO-DATA SET DESCRIPTION
      if(ipr.ge.3) then
         write(6,*)' List pseudo-data set used for error estimate at :',xob,' , ',yob
         write(6,*)' -------------------------------------------------'
         do 100 i=1,ndata
           write(6,*) tdata(i,1),tdata(i,2),tdata(i,3),tdata(i,4)
 100     continue
      endif
!c=======================================================================
      endif
      endif
      return
      end subroutine

! Procedure 65
! -----------

      subroutine fcorropti(x1,x2,y1,y2,corre)        
!C=============================================================================
      include'divapre.h'
      include'divainc.h'
      
      external euclidist                                                
      external bessk1
                                                                        
      r=euclidist(x1,x2,y1,y2)                                        
      eps=r/rl0
                                                                        
      if (eps.le.0.001) then                                                 
         corre=varbak                                                    
      else
         if(eps.lt.20.) then        
            corre=tbess(nint((eps-mod(eps,0.0005D0))/0.0005D0)) 
	 else 
	    corre=0
	 endif
      endif                                                             
!C=============================================================================
      return                                                            
      end subroutine                                                            
                                                      
! Procedure 66
! -----------

      subroutine tabess
!C=============================================================================
      include'divapre.h'
      include'divainc.h'
!C      dimension tbess(40000)
      external bessk1
      eps=0
      do 10 i=1,40000
      eps=eps+0.0005
         tbess(i)=varbak*eps*bessk1(eps)
 10   continue
!C=============================================================================
      return                                                            
      end subroutine                                                              

! Procedure 67
! -----------
      subroutine repeltest2 (tcoog,kconn,kntc,ncamax)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(3),jkntc(3),xn(3),yn(3)

!C MAKING THE KNTC 3D TABLE
!C (1) INITIALISE THE FIRST ELEMENTS OF KNTC
      ncamax=0
!c      write(6,*) 'into repel2'
      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue
!cmr      write(6,*)ncax,ncay,ncaz
!cmr      write(6,*)tmix,tmax,tmiy,tmay
!C (2) TAKE EACH TRIANGLE
      iel=0
 300  iel=iel+1

!C (2.1) TAKE THE THREE NODES OF THIS TRIANGLE
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)

!C (2.2) IN WHICH ELEMENT OF KNTC IS THIS TRIANGLE?
      do 400 i=1,3     
!cmr      ikntc(i)=((xn(i)-tmix-mod(xn(i),tlcx))/tlcx)+1
!cmr      jkntc(i)=((yn(i)-tmiy-mod(yn(i),tlcy))/tlcy)+1
!c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
!c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1
      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=max(1,min(jkntc(i),ncay))
 400  continue     
 
!C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.3) then
         goto 500 
      endif
!cmr      write(6,*) imii,imai,imij,imaj,iel
      if (imii.lt.0.or. &
         imai.lt.0.or.  &
         imij.lt.0.or.   &
         imaj.lt.0.or.    &
         imii.gt.ncax.or.  &
         imai.gt.ncax.or.   &
         imij.gt.ncay.or.    &
         imaj.gt.ncay) then
        write(6,*) 'wrong index', imii,imai,imij,imaj
        write(6,*)  imii,imai,imij,imaj
!cmr        stop
      endif
!C (4)  PUT THE NUMBER OF THE TRIANGLE IN THE FIRST FREE ELEMENTS CORRESPONDING
!C      TO THE BIGGEST RECTANGLE THAT CONTAINS THE TRIANGLE.

      do 800 i=imii,imai
         do 700 j=imij,imaj
         kntc(i,j,1)=kntc(i,j,1)+1
 700     continue
 800  continue

      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
      ncamax=1
      do i=1,ncax
      do j=1,ncay
        if(kntc(i,j,1).gt.ncamax) ncamax=kntc(i,j,1)
      enddo
      enddo
!C============================================================================
      return
      end subroutine

! Procedure 68
! -----------

      subroutine repeltest3 (tcoog,kconn,kntc,ncamax)
!C============================================================================
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      dimension kntc(ncax,ncay,*)
      dimension ikntc(4),jkntc(4),xn(4),yn(4)
      ncamax=0
      write(6,*) 'into repel3'
!C MAKING THE KNTC 3D TABLE
!C (1) INITIALISE THE FIRST ELEMENTS OF KNTC

      do 100 i=1,ncax
         do 100 j=1,ncay
                kntc(i,j,1)=0
 100     continue

!C (2) TAKE EACH ELEMENT
      iel=0
 300  iel=iel+1

!C (2.1) TAKE THE FOUR NODES OF THIS ELEMENT
      xn(1)=tcoog(kconn(iel,1),1)
      yn(1)=tcoog(kconn(iel,1),2)
      xn(2)=tcoog(kconn(iel,3),1)
      yn(2)=tcoog(kconn(iel,3),2)
      xn(3)=tcoog(kconn(iel,5),1)
      yn(3)=tcoog(kconn(iel,5),2)
      xn(4)=tcoog(kconn(iel,7),1)
      yn(4)=tcoog(kconn(iel,7),2)

!C (2.2) IN WHICH ELEMENT OF KNTC IS IT ?
      do 400 i=1,4
!c      ikntc(i)=((xn(i)-tmix-mod(xn(i)-tmix,tlcx))/tlcx)+1
!c      jkntc(i)=((yn(i)-tmiy-mod(yn(i)-tmiy,tlcy))/tlcy)+1
      ikntc(i)=((xn(i)-tmix)/tlcx)+1
      jkntc(i)=((yn(i)-tmiy)/tlcy)+1

      ikntc(i)=max(1,min(ikntc(i),ncax))
      jkntc(i)=min(1,min(jkntc(i),ncay))
 400  continue     
 
!C (3) COMPUTE THE MAX AND MIN OF IKNTC, JKNTC
      imai=ikntc(1)
      imii=ikntc(1)
      imaj=jkntc(1)
      imij=jkntc(1)
      i=1

 500  i=i+1
         if (ikntc(i).gt.imai) then 
            imai=ikntc(i)
         endif
         if (ikntc(i).lt.imii) then 
            imii=ikntc(i)
         endif
         if (jkntc(i).gt.imaj) then 
            imaj=jkntc(i)
         endif
         if (jkntc(i).lt.imij) then 
            imij=jkntc(i)
         endif
      if (i.lt.4) then
         goto 500 
      endif

!C  (4) PUT THE NUMBER OF THE ELEMENT IN THE FIRST FREE PLACE CORRESPONDING
!C      TO THE BIGGEST RECTANGLE THAT CONTAINS IT

      do 800 i=imii,imai
         do 700 j=imij,imaj
               kntc(i,j,1)=kntc(i,j,1)+1
 700     continue
 800  continue
      if (iel.lt.nelt) then
         j=-1
         goto 300
      endif
      ncamax=1
      do i=1,ncax
      do j=1,ncay
        if(kntc(i,j,1).gt.ncamax) ncamax=kntc(i,j,1)
      enddo
      enddo
!C============================================================================
      return
      end subroutine

! Procedure 69
! ------------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     - CALSH2 (evaluation of the shape functions at gauss points
!C        * EP2,EDPX2,EDPY2,EDDY2,EDDX2,EDXY2
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE CALSH2(TSHAG,XG,YG,NG,IPR)
!C     =========================
      include'divapre.h'
      DIMENSION EP(10),EDPX(10),EDPY(10),EDDX(10),EDDY(10),EDDXY(10),TSHAG(10,8,NG),XG(NG),YG(NG)
      ZERO=0.D0
!C
!C  REFERENCE TRIANGULAR SUB-ELEMENT
!C      3 NODES
!C
!C  CALCULATION OF SHAPE FUNCTION AT GAUSS INTEGRATION POINTS
!C
      do 25 ig=1,ng
         x=xg(ig)
         y=yg(ig)
         call ep2(x,y,ep)
         call edpx2(x,y,edpx)
         call edpy2(x,y,edpy)
         call eddx2(x,y,eddx)
         call eddy2(x,y,eddy)
         call edxy2(x,y,eddxy)
         do 30 i=1,10
            tshag(i,1,ig)=ep(i)
            tshag(i,2,ig)=edpx(i)
            tshag(i,3,ig)=edpy(i)
            tshag(i,4,ig)=eddx(i)
            tshag(i,5,ig)=eddy(i)
            tshag(i,6,ig)=eddxy(i)
 30      continue
 25   continue
!C
!C  CALCULATION OF X- AND Y- DERIVATIVES AT INTERIOR INTERFACE 1
!C
      x=0.5
      y=zero
      call edpx2(x,y,edpx)
      call edpy2(x,y,edpy)
      do 40 i=1,10
         tshag(i,7,1)=edpx(i)
         tshag(i,8,1)=edpy(i)
 40   continue
!C
!C  CALCULATION OF X- AND Y- DERIVATIVES AT INTERIOR INTERFACE 2
!C
      x=zero
      y=0.5
      call edpx2(x,y,edpx)
      call edpy2(x,y,edpy)
      do 50 i=1,10
         tshag(i,7,2)=edpx(i)
         tshag(i,8,2)=edpy(i)
 50   continue
      if(ipr.gt.4) then
         write(6,*) '+++++++++++++++++++++++++++++++++++++++++++++++ '
         write(6,*) '   Shape Functions at Gauss Integration Points  '
         write(6,*) '   -------------------------------------------  '
         do 100 i=1,10
            write(6,*) (tshag(i,1,k),k=1,ng)
 100     continue
         write(6,*) '   Dx Shape Functions at Gauss Integration Points'
         write(6,*) '   ----------------------------------------------'
         do 110 i=1,10
            write(6,*) (tshag(i,2,k),k=1,ng)
 110     continue
         write(6,*) '   Dy Shape Functions at Gauss Integration Points'
         write(6,*) '   ----------------------------------------------'
         do 120 i=1,10
            write(6,*) (tshag(i,3,k),k=1,ng)
 120     continue
         write(6,*) '  D2x Shape Functions at Gauss Integration Points'
         write(6,*) '  -----------------------------------------------'
         do 130 i=1,10
            write(6,*) (tshag(i,4,k),k=1,ng)
 130     continue
         write(6,*) '  D2y Shape Functions at Gauss Integration Points'
         write(6,*) '  -----------------------------------------------'
         do 140 i=1,10
            write(6,*) (tshag(i,5,k),k=1,ng)
 140     continue
         write(6,*) ' D2xy Shape Functions at Gauss Integration Points'
         write(6,*) ' ------------------------------------------------'
         do 150 i=1,10
            write(6,*) (tshag(i,6,k),k=1,ng)
 150     continue
         write(6,*) 'Dx Shape Functions at interior interfaces 1 and 2'
         write(6,*) '-------------------------------------------------'
         do 160 i=1,10
            write(6,*) (tshag(i,7,k),k=1,2)
 160     continue
         write(6,*) 'Dy Shape Functions at interior interfaces 1 and 2'
         write(6,*) '-------------------------------------------------'
         do 170 i=1,10
            write(6,*) (tshag(i,8,k),k=1,2)
 170     continue
         write(6,*) '+++++++++++++++++++++++++++++++++++++++++++++++++'
      endif
      RETURN
      END subroutine

! Procedure 70
! ------------



      SUBROUTINE EP2(R,S,VP)
!C     =======================
      include'divapre.h'
      DIMENSION VP(10)
      UN=1.D0
      DEUX=2.D0
      TROIS=3.D0
      SIX=6.D0
      RAC2=2.D0*SQRT(2.D0)
      VP(1)=UN-SIX*R*S-TROIS*R*R-TROIS*S*S+SIX*R*R*S+SIX*R*S*S+DEUX*R*R*R+DEUX*S*S*S
      VP(2)=R-DEUX*R*S-DEUX*R*R+DEUX*R*R*S+R*S*S+R*R*R
      VP(3)=S-DEUX*S*R-DEUX*S*S+DEUX*S*S*R+S*R*R+S*S*S
      VP(4)=TROIS*R*S+TROIS*R*R-TROIS*R*R*S-TROIS*R*S*S-DEUX*R*R*R
      VP(5)=-R*S/DEUX-R*R+R*R*S/DEUX+R*S*S/DEUX+R*R*R
      VP(6)=R*S*TROIS/DEUX-R*R*S/DEUX-R*S*S*TROIS/DEUX
      VP(7)=TROIS*S*R+TROIS*S*S-TROIS*S*S*R-TROIS*S*R*R-DEUX*S*S*S
      VP(8)=S*R*TROIS/DEUX-S*S*R/DEUX-S*R*R*TROIS/DEUX
      VP(9)=-S*R/DEUX-S*S+S*S*R/DEUX+S*R*R/DEUX+S*S*S
      VP(10)=-RAC2*R*S+RAC2*R*R*S+RAC2*R*S*S
      RETURN
      END subroutine

! Procedure 71
! ------------

      SUBROUTINE EDPX2(R,S,VDPX)
!C     ===========================
      include'divapre.h'
      DIMENSION VDPX(10)
      UN=1.D0
      DEUX=2.D0
      TROIS=3.D0
      QUATRE=4.D0
      SIX=6.D0
      RAC2=2.D0*SQRT(2.D0)
      VDPX(1)=-SIX*R-SIX*S+SIX*DEUX*R*S+SIX*S*S+SIX*R*R
      VDPX(2)=UN-DEUX*S-QUATRE*R+QUATRE*R*S+S*S+TROIS*R*R
      VDPX(3)=-DEUX*S+DEUX*R*S+DEUX*S*S
      VDPX(4)=TROIS*S+SIX*R-SIX*R*S-TROIS*S*S-SIX*R*R
      VDPX(5)=-S/DEUX-DEUX*R+R*S+S*S/DEUX+TROIS*R*R
      VDPX(6)=S*TROIS/DEUX-R*S-S*S*TROIS/DEUX
      VDPX(7)=TROIS*S-SIX*R*S-TROIS*S*S
      VDPX(8)=S*TROIS/DEUX-TROIS*R*S-S*S/DEUX
      VDPX(9)=-S/DEUX+R*S+S*S/DEUX
      VDPX(10)=-RAC2*S+DEUX*RAC2*R*S+RAC2*S*S
      RETURN
      END subroutine

! Procedure 72
! ------------



      SUBROUTINE EDPY2(R,S,VDPY)
!C     ===========================
      include'divapre.h'
      DIMENSION VDPY(10)
      UN=1.D0
      DEUX=2.D0
      TROIS=3.D0
      QUATRE=4.D0
      SIX=6.D0
      RAC2=2.D0*SQRT(2.D0)
      VDPY(1)=-SIX*R-SIX*S+SIX*DEUX*R*S+SIX*S*S+SIX*R*R
      VDPY(2)=-DEUX*R+DEUX*S*R+DEUX*R*R
      VDPY(3)=UN-DEUX*R-QUATRE*S+QUATRE*S*R+R*R+TROIS*S*S
      VDPY(4)=TROIS*R-SIX*S*R-TROIS*R*R
      VDPY(5)=-R/DEUX+S*R+R*R/DEUX
      VDPY(6)=R*TROIS/DEUX-TROIS*S*R-R*R/DEUX
      VDPY(7)=TROIS*R+SIX*S-SIX*S*R-TROIS*R*R-SIX*S*S
      VDPY(8)=R*TROIS/DEUX-S*R-R*R*TROIS/DEUX
      VDPY(9)=-R/DEUX-DEUX*S+S*R+R*R/DEUX+TROIS*S*S
      VDPY(10)=-RAC2*R+DEUX*RAC2*S*R+RAC2*R*R
      RETURN
      END subroutine

! Procedure 73
! ------------

      SUBROUTINE EDDY2(R,S,VDDPY)
!C     ============================
      include'divapre.h'
      DIMENSION VDDPY(10)
      DEUX=2.D0
      TROIS=3.D0
      QUATRE=4.D0
      SIX=6.D0
      DOUZE=12.D0
      RAC2=2.D0*SQRT(2.D0)
      VDDPY(1)=-SIX+DOUZE*S+DOUZE*R
      VDDPY(2)=DEUX*R
      VDDPY(3)=-QUATRE+QUATRE*R+SIX*S
      VDDPY(4)=-SIX*R
      VDDPY(5)=R
      VDDPY(6)=-TROIS*R
      VDDPY(7)=SIX-SIX*R-DOUZE*S
      VDDPY(8)=-R
      VDDPY(9)=-DEUX+R+SIX*S
      VDDPY(10)=DEUX*RAC2*R
      RETURN
      END subroutine

! Procedure 74
! ------------

      SUBROUTINE EDDX2(R,S,VDDPX)
!C     ============================
      include'divapre.h'
      DIMENSION VDDPX(10)
      DEUX=2.D0
      TROIS=3.D0
      QUATRE=4.D0
      SIX=6.D0
      DOUZE=12.D0
      RAC2=2.D0*SQRT(2.D0)
      VDDPX(1)=-SIX+DOUZE*S+DOUZE*R
      VDDPX(2)=-QUATRE+QUATRE*S+SIX*R
      VDDPX(3)=DEUX*S
      VDDPX(4)=SIX-SIX*S-DOUZE*R
      VDDPX(5)=-DEUX+S+SIX*R
      VDDPX(6)=-S
      VDDPX(7)=-SIX*S
      VDDPX(8)=-TROIS*S
      VDDPX(9)=S
      VDDPX(10)=DEUX*RAC2*S
      RETURN
      END subroutine
      
! Procedure 75
! ------------
      

      SUBROUTINE EDXY2(R,S,VDPXY)
!C     ============================
      include'divapre.h'
      DIMENSION VDPXY(10)
      UN=1.D0
      DEUX=2.D0
      TROIS=3.D0
      QUATRE=4.D0
      SIX=6.D0
      DOUZE=12.D0
      RAC2=2.D0*SQRT(2.D0)
      VDPXY(1)=-SIX+DOUZE*R+DOUZE*S
      VDPXY(2)=-DEUX+QUATRE*R+DEUX*S
      VDPXY(3)=-DEUX+QUATRE*S+DEUX*R
      VDPXY(4)=TROIS-SIX*R-SIX*S
      VDPXY(5)=-UN/DEUX+R+S
      VDPXY(6)=TROIS/DEUX-R-TROIS*S
      VDPXY(7)=TROIS-SIX*R-SIX*S
      VDPXY(8)=TROIS/DEUX-TROIS*R-S
      VDPXY(9)=-UN/DEUX+R+S
      VDPXY(10)=-RAC2+DEUX*RAC2*R+DEUX*RAC2*S
      RETURN
      END subroutine
      
! Procedure 76
! ------------
      
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  SOLVER (MODULE)
!C     -  CALKEL : calculating and assembling elementary matrices
!C     -  CDIR   : calculating cos-directions
!C     -  CKELE2 : integrating elementary matrices when ITYP=2
!C     -  CKELE3 : integrating elementary matrices when ITYP=3
!C     -  CKSEL2 : integrating sub-elementary matrices when ITYP=2
!C     -  FIXBCD : fixe the boundary conditions (Dirichlet type)
!C     -  LOCSE2 : localisation of sub-elements in the ITYP=2 element
!C     -  LOCSE3 : localisation of sub-elements in the ITYP=3 element
!C     -  IMPSOL : print the solution at principal connectors
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             SOLVER MODULE                            C
!C       Build and solve the global linear finite element system        C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine solver(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  INPUT OF GENERAL DATA
!C
      write(6,*) 'into solver',ipr
      read(10,*) istf 
      if (istf.ne.0) then
      write(6,*) 'Stiffness parameter ISTF',istf
      endif
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> TUPPE   : UPPER VECTOR CONTAINING THE GLOBAL LINEAR SYSTEM
!C  ==> TLOWE   : LOWER VECTOR OF THE GLOBAL LINEAR SYSTEM (IF ISYM=0)
!C  ==> TDIAG   : DIAGONAL VECTOR OF THE GLOBAL LINEAR SYSTEM
!C  ==> TRHSG   : RIGHT HAND SIDE OF THE GLOBAL LINEAR SYSTEM
!C  ==> TKELE   : ELEMENTARY MATRIX (NDDLE*NDDLE)
!C  ==> TRHSE   : RIGHT HAND SIDE OF THE ELEMENTARY SYSTEM
!C
      call allody(nelt,1,'tstif',ltstif,ipr)
      call allody(nterm,1,'tuppe',ltuppe,ipr)
      if(isym.eq.0) then
         call allody(1,1,'tlowe',ltlowe,ipr)
                    else
         call allody(nterm,1,'tlowe',ltlowe,ipr)
      endif
      call allody(nddlt,1,'tdiag',ltdiag,ipr)
      call allody(nddlt,1,'trhsg',ltrhsg,ipr)
      call allody(nddle*nddle,1,'tkele',ltkele,ipr)
      call allody(nddle,1,'trhse',ltrhse,ipr)
!C
!C  CALCULATION OF ELEMENTARY MATRICES AND INTEGRATION
!C              IN THE GLOBAL SYSTEM
!C
      call calkel(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),s(ltcoog),s(ltkele),s(ltrhse),l(lkconn),l(lkskyh),s(ltstif),ipr)
!C
!C  FIX BOUNDARY CONDITIONS
!C
      if(ltcndi.eq.0) then
      bidon=0
      ibidon=0
      call fixbcd(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),l(lklink),l(lkskyh),bidon,ibidon)

                       else

      call fixbcd(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),l(lklink),l(lkskyh),s(ltcndi),l(lkcndi))
      endif
      
!C
!C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
!C
      ifac=1
      isol=1
      mp=6
      call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,mp,ifac,isol,isym,energ)
      if(ipr.ge.1) then
         call impsol(s(ltcoog),l(lklink),s(ltrhsg))
      endif
      return
      end subroutine
      
! Procedure 77
! ------------
      




      subroutine calkel(tuppe,tlowe,tdiag,trhsg,tcoog,tkele,trhse,kconn,kskyh,tstif,ipr)
!C
!C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY MATRICES AND
!C  ASSEMBLING IN THE GLOBAL SYSTEM
!C
      include'divapre.h'
      include'divainc.h'
      dimension tuppe(nterm),tlowe(nterm),tdiag(nddlt),trhsg(nddlt),   &
               tcoog(nnt1,2),tkele(nddle,nddle),trhse(nddle),          &
               kconn(nelt,nnel),kskyh(nddlt+1),tstif(nelt),             &
               xg(16),yg(16),wg(16)
      one=1.0D0
      three=3.0D0
      five=5.0D0
!C
!C  READ ELEMENTARY STIFNESSES WHEN ISTF = 1 (PSEUDO-MODIF. OF TOPOLOGY)
!C  DEFAULT VALUE = 1
!C
      do 2 iel=1,nelt
         tstif(iel)=one        
 2    continue
      if(istf.eq.1) then
         do 5 iel=1,nelt
            read(60,*)tstif(iel)
 5       continue
      endif
!C
!C  EVALUATION OF SHAPE FUNCTIONS AT GAUSS INTEGRATION POINTS
!C  AND AT INTERIOR INTERFACES BETWEEN SUB-ELEMENTS
!C   Maximum : 16 Gauss Integration Points
!C======================================================================
!C  SHAPE FUNCTIONS WHEN ITYP = 2 OR 3: NG = 4 (HAMMER RULE)
!C  ALLOCATION OF STORAGE ARRAYS:
!C   ==> TSHAG(I,J,K) = VALUE OF SHAPE FUNCTION I, IRULE=J,
!C       GAUSS P OR INTERIOR INTERFACE = K
!C         (IRULE=:1=>VALUE; 2=>DX; 3=>DY; 4=>DDX; 5=>DDY; 6=>DDXY;
!C          7=>  DX AT INTERIOR INTERFACES;
!C          8=>  DY AT INTERIOR INTERFACES)
!C   ==> KLOCS(I,J) = ARRAY FOR LOCALISATION OF SUB-ELEMENTS IN ELEMENTS
!C
!C  WHEN ITYP.EQ.3 ...
!C
      if(ityp.eq.2) then
         ng=4
         call allody(10*8*ng,1,'tshag',ltshag,ipr)
         call allody(30,0,'klocs',lklocs,ipr)
!C
!C  NUMERICAL INTEGRATION : HAMMER METHOD ON A TRIANGLE (Dhatt & Touzot)
!C    FOR 3rd ORDER POLYNOMIAL BASIS: 4 Gauss Points
!C
         xg(1)=one/three
         yg(1)=one/three
         wg(1)=-27./96.
         xg(2)=one/five
         yg(2)=one/five
         wg(2)=25./96.
         xg(3)=three/five
         yg(3)=one/five
         wg(3)=25./96.
         xg(4)=one/five
         yg(4)=three/five
         wg(4)=25./96.
         call calsh2(s(ltshag),xg,yg,ng,ipr)
!C
!C  CONSTRUCTION OF LOCALISATION ARRAY FOR SUB-ELEMENTS
!C
         call locse2(l(lklocs))
      endif
!C
!C  WHEN ITYP.EQ.3 ...
!C
      if(ityp.eq.3) then
         ng=4
         call allody(10*8*ng,1,'tshag',ltshag,ipr)
         call allody(40,0,'klocs',lklocs,ipr)
!C
!C  NUMERICAL INTEGRATION : HAMMER METHOD ON A TRIANGLE (Dhatt & Touzot)
!C    FOR 3rd ORDER POLYNOMIAL BASIS: 4 Gauss Points
!C
         xg(1)=one/three
         yg(1)=one/three
         wg(1)=-27./96.
         xg(2)=one/five
         yg(2)=one/five
         wg(2)=25./96.
         xg(3)=three/five
         yg(3)=one/five
         wg(3)=25./96.
         xg(4)=one/five
         yg(4)=three/five
         wg(4)=25./96.
         call calsh2(s(ltshag),xg,yg,ng,ipr)
!C
!C  CONSTRUCTION OF LOCALISATION ARRAY FOR SUB-ELEMENTS
!C
         call locse3(l(lklocs))
      endif
      if(isym.eq.0) then
         nspace=nddle*(nddle+1)/2
                    else
         nspace=nddle*nddle
      endif
      call allody(nspace,1,'tvele',ltvele,ipr)
!C
!C  EVALUATION OF ELEMENTARY MATRIX WHEN ITYP = 2 OR 3
!C
!C  OPEN FILE FOR STORAGE OF CONDENSATION VECTORS (FOR DECONDENSATION)
!C
!c      if(ityp.eq.2.or.ityp.eq.3) then
!c         open(32,file='kele2.cnd',recl=nddle*3*iprc,form='unformatted',
!c     &        access='direct')
!c      endif
!C REPLACE by allody for TRKELE and pointer LRKELE
      call allody(3*12*NELT,1,'trkele',lrkele,ipr)
!C now s(lrkele) for TRKELE
      do 10 iel=1,nelt
         if(ityp.eq.2) then
            if(ltprop.eq.0) then
            bidon=0
            call ckele2 (iel,tstif(iel),tcoog,kconn,tkele,trhse, &
                        l(lklocs),                                &
                        bidon,wg,ipr,s(lrkele))
            else
            call ckele2 (iel,tstif(iel),tcoog,kconn,tkele,trhse,   &
                        l(lklocs),                                  &
                        s(ltprop),wg,ipr,s(lrkele))
            endif
         endif
         if(ityp.eq.3) then
         if(ltprop.eq.0) then
            bidon=0
            call ckele3 (iel,tstif(iel),tcoog,kconn,tkele,trhse,        &
                        l(lklocs),                                     &
                        bidon,s(ltcele),wg,ipr,s(lrkele))
                         else
            call ckele3 (iel,tstif(iel),tcoog,kconn,tkele,trhse,       &
                        l(lklocs),                                    &
                        s(ltprop),s(ltcele),wg,ipr,s(lrkele))
         endif
         endif
         if(ipr.ge.2) write(6,*) ' ...assembling element',iel
         IF(IPR.GE.4) then
            WRITE (6,405) iel
         endif
 405     FORMAT(///,T10,' ELEMENTARY  MATRIX FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(tkele,nddle,nddle,nddle,6)
         IF(IPR.GE.4) then
            WRITE (6,406) iel
         endif
 406     FORMAT(///,T10,' ELEMENTARY  RHS  FOR ELEMENT ',I5,///)
         IF(IPR.GE.4) CALL IMPMAT(trhse,nddle,1,nddle,6)
!C
!C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
!C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
!C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
!C
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         call append(tkele,nddle,s(ltvele),nspace,isym)
         ikg=1
         ifg=1
!c         write(6,*) 'Into assel ',nddlt,kskyh(1)
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue
      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
      return
      end subroutine
      
! Procedure 78
! ------------
      



      subroutine cdir(dx,dy,rn,sn)
      include'divapre.h'
      dist=sqrt(dx*dx+dy*dy)
      rn=-dy/dist
      sn=dx/dist
      return
      end subroutine



! Procedure 79
! ------------

      subroutine ckele2 (iel,stiff,tcoog,kconn,tkele,trhse,loces,tprop,wg,ipr,TRKELE)
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),tkele(12,12),trhse(12),kconn(nelt,nnel),x(0:3),y(0:3),wg(ng),tprop(nnt1,nnpr),u(0:3),v(0:3)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tke(15,15),tge(15),tkse(10,10),tgse(10),loces(3,10),&
               derx1(10),dery1(10),derx2(10),dery2(10),          &
               tcond(3,15),ro(3,3),tr(3,12),rw(3,12),romin(3,3) &
               ,TRKELE(3,12,*),rll(0:3)
      toler=0.0001
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS) - INIT.
!C  CALCULATE NORMAL COMPONENTS OF INTERNAL INTERFACES
!C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      if(itcs.eq.1.or.itcs.eq.3) then
         u(1)=tprop(kconn(iel,1),1)
         v(1)=tprop(kconn(iel,1),2)
         u(2)=tprop(kconn(iel,3),1)
         v(2)=tprop(kconn(iel,3),2)
         u(3)=tprop(kconn(iel,5),1)
         v(3)=tprop(kconn(iel,5),2)
         u(0)=(u(1)+u(2)+u(3))/3.
         v(0)=(v(1)+v(2)+v(3))/3.
      endif
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(0)=(rll(1)+rll(2)+rll(3))/3.
      endif
      
      dy=y(1)-y(0)
      dx=x(1)-x(0)
      call cdir(dx,dy,rn,sn)
      rn1=rn
      sn1=sn
      dy=y(2)-y(0)
      dx=x(2)-x(0)
      call cdir(dx,dy,rn,sn)
      rn2=rn
      sn2=sn
      dy=y(3)-y(0)
      dx=x(3)-x(0)
      call cdir(dx,dy,rn,sn)
      rn3=rn
      sn3=sn
      zero=0.D0
      do 10 i=1,15
         tge(i)=zero
         do 12 j=1,3
            tcond(j,i)=zero
 12      continue
         do 20 j=1,15
         tke(i,j)=zero
 20      continue
 10   continue
!C
!C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
!C
      x0=x(0)
      y0=y(0)
      u0=u(0)
      v0=v(0)
      rll0=rll(0)
!C
!C  First sub-element
!C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(1)
         v1=v(1)
         u2=u(2)
         v2=v(2)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
      call cksel2(1,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)   &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,      &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
      tcond(1,loces(1,i))=tcond(1,loces(1,i))+rn1*derx1(i)+sn1*dery1(i)
      tcond(2,loces(1,i))=tcond(2,loces(1,i))-rn2*derx2(i)-sn2*dery2(i)
      do 115 j=1,10
      tke(loces(1,i),loces(1,j))=tke(loces(1,i),loces(1,j))+tkse(i,j)
 115  continue
 110  continue
!C
!C  Second sub-element
!C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(2)
         v1=v(2)
         u2=u(3)
         v2=v(3)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cksel2(2,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)     &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,         &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
      tcond(2,loces(2,i))=tcond(2,loces(2,i))+rn2*derx1(i)+sn2*dery1(i)
      tcond(3,loces(2,i))=tcond(3,loces(2,i))-rn3*derx2(i)-sn3*dery2(i)
      do 125 j=1,10
      tke(loces(2,i),loces(2,j))=tke(loces(2,i),loces(2,j))+tkse(i,j)
 125  continue
 120  continue
!C
!C  Third sub-element
!C
      x1=x(3)
      y1=y(3)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(3)
         v1=v(3)
         u2=u(1)
         v2=v(1)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(1)
      endif
      call cksel2(3,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)  &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,      &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
      tcond(3,loces(3,i))=tcond(3,loces(3,i))+rn3*derx1(i)+sn3*dery1(i)
      tcond(1,loces(3,i))=tcond(1,loces(3,i))-rn1*derx2(i)-sn1*dery2(i)
      do 135 j=1,10
      tke(loces(3,i),loces(3,j))=tke(loces(3,i),loces(3,j))+tkse(i,j)
 135  continue
 130  continue
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      do 140 i=1,3
         do 145 j=1,3
            ro(i,j)=tcond(i,j+12)
 145     continue
 140  continue
      if(ipr.gt.6) then
         write(6,*)'   RO MATRIX '
         call impmat(ro,3,3,3,6)
         write(6,*)'   ELEMENTARY MATRIX BEFORE REDUCTION (15*15) '
         call impmat(tke,15,15,15,6)
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (15) '
         call impmat(tge,15,1,15,6)
         write(6,*)'   REDUCTION VECTOR (3*15) '
         call impmat(tcond,3,15,3,6)
      endif
!C
!C  INVERSE OF RO
!C
      romin(1,1)=ro(2,2)*ro(3,3)-ro(3,2)*ro(2,3)
      romin(1,2)=ro(3,1)*ro(2,3)-ro(2,1)*ro(3,3)
      romin(1,3)=ro(2,1)*ro(3,2)-ro(3,1)*ro(2,2)
      romin(2,1)=ro(1,3)*ro(3,2)-ro(3,3)*ro(1,2)
      romin(2,2)=ro(1,1)*ro(3,3)-ro(3,1)*ro(1,3)
      romin(2,3)=ro(1,2)*ro(3,1)-ro(3,2)*ro(1,1)
      romin(3,1)=ro(1,2)*ro(2,3)-ro(2,2)*ro(1,3)
      romin(3,2)=ro(1,3)*ro(2,1)-ro(2,3)*ro(1,1)
      romin(3,3)=ro(2,2)*ro(1,1)-ro(2,1)*ro(1,2)
      detro=romin(1,1)*ro(1,1)+romin(1,2)*ro(1,2)+romin(1,3)*ro(1,3)
      if(abs(detro).lt.toler) then
         write(6,*) ' %%% ERROR - CKELE2 : DETRO = ',detro
         stop
      endif
      do 141 i=1,3
        do 142 j=1,3
           ro(i,j)=romin(j,i)/detro
 142    continue
 141  continue
      if(ipr.gt.6) then
         write(6,*)'   RO determinant = ',detro
         write(6,*)'   RO INVERSE MATRIX '
         call impmat(ro,3,3,3,6)
      endif
      do 150 i=1,3
         do 155 j=1,12
            tr(i,j)=zero
            do 157 k=1,3
               tr(i,j)=tr(i,j)-ro(i,k)*tcond(k,j)
 157        continue
 155     continue
 150  continue
 
!CJMB
!C       write(32,rec=iel) tr
!c       if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c       endif
       IJMB=1
       if(ipr.gt.5) write(6,*) 'saving trkele'
        do i=1,3
         do j=1,12
          trkele(i,j,iel)=tr(i,j)
!c          ijmbw(iel)=1
          enddo
        enddo
!C
!C  REDUCTION OF ELEMENTARY MATRIX (15*15) ==> (12*12)
!C
      do 158 i=1,3
         do 159 j=1,12
            rw(i,j)=zero
            do 156 k=1,3
               rw(i,j)=rw(i,j)+tke(i+12,k+12)*tr(k,j)
 156        continue
 159     continue
 158  continue
      do 160 i=1,12
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+12)
 164     continue
         trhse(i)=tge(i)+gimp
         do 165 j=1,12
            tcd1=zero
            tcd2=zero
            tcd3=zero
            do 161 k=1,3
               tcd1=tcd1+tr(k,i)*tke(12+k,j)
 161        continue
            do 162 k=1,3
               tcd2=tcd2+tke(i,k+12)*tr(k,j)
 162        continue
            do 163 k=1,3
               tcd3=tcd3+tr(k,i)*rw(k,j)
 163        continue
         tkele(i,j)=tke(i,j)+tcd1+tcd2+tcd3
 165  continue
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         do 200 i=1,12
            tkele(i,10)=-tkele(i,10)
            tkele(10,i)=-tkele(10,i)
 200     continue
            trhse(10)=-trhse(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         do 210 i=1,12
            tkele(i,11)=-tkele(i,11)
            tkele(11,i)=-tkele(11,i)
 210     continue
            trhse(11)=-trhse(11)
      endif
      if(kconn(iel,1).lt.kconn(iel,5)) then
         do 220 i=1,12
            tkele(i,12)=-tkele(i,12)
            tkele(12,i)=-tkele(12,i)
 220     continue
            trhse(12)=-trhse(12)
      endif
      return
      end subroutine

! Procedure 80
! ------------


      subroutine ckele3 (iel,stiff,tcoog,kconn,tkele,trhse,loces,tprop,tcele,wg,ipr,TRKELE)
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),tkele(16,16),trhse(16),kconn(nelt,nnel), &
               x(0:4),y(0:4),wg(ng),tprop(nnt1,nnpr),u(0:4),v(0:4),    &
               tcele(nelt,2)                                            &
               ,TRKELE(3,12,*)
!C
!C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
!C
      dimension tke(19,19),tge(19),tkse(10,10),tgse(10),loces(4,10), &
               derx1(10),dery1(10),derx2(10),dery2(10),              &
               tcond(3,19),ro(3,3),tr(3,16),rw(3,16),romin(3,3)       &
                              ,rll(0:4)
      toler=0.0001
!C
!C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS) - INIT.
!C  CALCULATE NORMAL COMPONENTS OF INTERNAL INTERFACES
!C
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      if(itcs.eq.1.or.itcs.eq.3) then
         u(1)=tprop(kconn(iel,1),1)
         v(1)=tprop(kconn(iel,1),2)
         u(2)=tprop(kconn(iel,3),1)
         v(2)=tprop(kconn(iel,3),2)
         u(3)=tprop(kconn(iel,5),1)
         v(3)=tprop(kconn(iel,5),2)
         u(4)=tprop(kconn(iel,7),1)
         v(4)=tprop(kconn(iel,7),2)
         u(0)=(u(1)+u(2)+u(3)+u(4))/4.
         v(0)=(v(1)+v(2)+v(3)+v(4))/4.
      endif
      if(itcs.eq.2) then
      rll(1)=tprop(kconn(iel,1),1)
      rll(2)=tprop(kconn(iel,3),1)
      rll(3)=tprop(kconn(iel,5),1)
      rll(4)=tprop(kconn(iel,7),1)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      if(itcs.eq.3) then
      rll(1)=tprop(kconn(iel,1),3)
      rll(2)=tprop(kconn(iel,3),3)
      rll(3)=tprop(kconn(iel,5),3)
      rll(4)=tprop(kconn(iel,7),3)
      rll(0)=(rll(1)+rll(2)+rll(3)+rll(4))/4.
      endif
      
      
      dy=y(1)-y(0)
      dx=x(1)-x(0)
      call cdir(dx,dy,rn,sn)
      rn1=rn
      sn1=sn
      dy=y(2)-y(0)
      dx=x(2)-x(0)
      call cdir(dx,dy,rn,sn)
      rn2=rn
      sn2=sn
      dy=y(3)-y(0)
      dx=x(3)-x(0)
      call cdir(dx,dy,rn,sn)
      rn3=rn
      sn3=sn
      dy=y(4)-y(0)
      dx=x(4)-x(0)
      call cdir(dx,dy,rn,sn)
      rn4=rn
      sn4=sn
      zero=0.D0
      do 10 i=1,19
         tge(i)=zero
         do 12 j=1,3
            tcond(j,i)=zero
 12      continue
         do 20 j=1,19
         tke(i,j)=zero
 20      continue
 10   continue
!C
!C  INTEGRATION OF THE 4 (10*10) SUB-ELEMENT MATRICES
!C
      x0=x(0)
      y0=y(0)
      u0=u(0)
      v0=v(0)
      rll0=rll(0)
!C
!C  First sub-element
!C
      x1=x(1)
      y1=y(1)
      x2=x(2)
      y2=y(2)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(1)
         v1=v(1)
         u2=u(2)
         v2=v(2)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(1)
         rll2=rll(2)
      endif
      call cksel2(1,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)  &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,     &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
      tcond(1,loces(1,i))=tcond(1,loces(1,i))+rn1*derx1(i)+sn1*dery1(i)
      tcond(2,loces(1,i))=tcond(2,loces(1,i))-rn2*derx2(i)-sn2*dery2(i)
      do 115 j=1,10
      tke(loces(1,i),loces(1,j))=tke(loces(1,i),loces(1,j))+tkse(i,j)
 115  continue
 110  continue
!C
!C  Second sub-element
!C
      x1=x(2)
      y1=y(2)
      x2=x(3)
      y2=y(3)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(2)
         v1=v(2)
         u2=u(3)
         v2=v(3)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(2)
         rll2=rll(3)
      endif
      call cksel2(2,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)   &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,      &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
      tcond(2,loces(2,i))=tcond(2,loces(2,i))+rn2*derx1(i)+sn2*dery1(i)
      tcond(3,loces(2,i))=tcond(3,loces(2,i))-rn3*derx2(i)-sn3*dery2(i)
      do 125 j=1,10
      tke(loces(2,i),loces(2,j))=tke(loces(2,i),loces(2,j))+tkse(i,j)
 125  continue
 120  continue
!C
!C  Third sub-element
!C
      x1=x(3)
      y1=y(3)
      x2=x(4)
      y2=y(4)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(3)
         v1=v(3)
         u2=u(4)
         v2=v(4)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(3)
         rll2=rll(4)
      endif
      call cksel2(3,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)   &
                 ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,       &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
      tcond(3,loces(3,i))=tcond(3,loces(3,i))+rn3*derx1(i)+sn3*dery1(i)
      do 135 j=1,10
      tke(loces(3,i),loces(3,j))=tke(loces(3,i),loces(3,j))+tkse(i,j)
 135  continue
 130  continue
!C
!C  Fourth sub-element
!C
      x1=x(4)
      y1=y(4)
      x2=x(1)
      y2=y(1)
      if(itcs.eq.1.or.itcs.eq.3) then
         u1=u(4)
         v1=v(4)
         u2=u(1)
         v2=v(1)
      endif
      if(itcs.eq.2.or.itcs.eq.3) then
         rll1=rll(4)
         rll2=rll(1)
      endif
      call cksel2(4,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata),l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1, &
                  derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
      tcond(1,loces(4,i))=tcond(1,loces(4,i))-rn1*derx2(i)-sn1*dery2(i)
      do 136 j=1,10
      tke(loces(4,i),loces(4,j))=tke(loces(4,i),loces(4,j))+tkse(i,j)
 136  continue
 131  continue
!C
!C             AND ELIMINATING THE D.O.F AT THE CENTER
!C                       MATRIX CONDENSATION
!C       (see method in: Brasseur, 1993, Ph. D. dissertation)
!C
      do 140 i=1,3
         do 145 j=1,3
            ro(i,j)=tcond(i,j+16)
 145     continue
 140  continue
      if(ipr.gt.6) then
         write(6,*)'   RO MATRIX '
         call impmat(ro,3,3,3,6)
         write(6,*)'   ELEMENTARY MATRIX BEFORE REDUCTION (19*19) '
         call impmat(tke,19,19,19,6)
         write(6,*)'   ELEMENTARY RHS BEFORE REDUCTION (19) '
         call impmat(tge,19,1,19,6)
         write(6,*)'   REDUCTION VECTOR (3*19) '
         call impmat(tcond,3,19,3,6)
      endif
!C
!C  INVERSE OF RO
!C
      romin(1,1)=ro(2,2)*ro(3,3)-ro(3,2)*ro(2,3)
      romin(1,2)=ro(3,1)*ro(2,3)-ro(2,1)*ro(3,3)
      romin(1,3)=ro(2,1)*ro(3,2)-ro(3,1)*ro(2,2)
      romin(2,1)=ro(1,3)*ro(3,2)-ro(3,3)*ro(1,2)
      romin(2,2)=ro(1,1)*ro(3,3)-ro(3,1)*ro(1,3)
      romin(2,3)=ro(1,2)*ro(3,1)-ro(3,2)*ro(1,1)
      romin(3,1)=ro(1,2)*ro(2,3)-ro(2,2)*ro(1,3)
      romin(3,2)=ro(1,3)*ro(2,1)-ro(2,3)*ro(1,1)
      romin(3,3)=ro(2,2)*ro(1,1)-ro(2,1)*ro(1,2)
      detro=romin(1,1)*ro(1,1)+romin(1,2)*ro(1,2)+romin(1,3)*ro(1,3)
      if(abs(detro).lt.toler) then
         write(6,*) ' %%% ERROR - CKELE3 : DETRO = ',detro
         stop
      endif
      do 141 i=1,3
        do 142 j=1,3
           ro(i,j)=romin(j,i)/detro
 142    continue
 141  continue
      if(ipr.gt.6) then
         write(6,*)'   RO determinant = ',detro
         write(6,*)'   RO INVERSE MATRIX '
         call impmat(ro,3,3,3,6)
      endif
      do 150 i=1,3
         do 155 j=1,16
            tr(i,j)=zero
            do 157 k=1,3
               tr(i,j)=tr(i,j)-ro(i,k)*tcond(k,j)
 157        continue
 155     continue
 150  continue
!C JMB
!C      write(32,rec=iel) tr
!c      if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
        do i=1,3
         do j=1,12
          trkele(i,j,iel)=tr(i,j)
          enddo
        enddo
!C JMB
!C
!C  REDUCTION OF ELEMENTARY MATRIX (19*19) ==> (16*16)
!C
      do 158 i=1,3
         do 159 j=1,16
            rw(i,j)=zero
            do 156 k=1,3
               rw(i,j)=rw(i,j)+tke(i+16,k+16)*tr(k,j)
 156        continue
 159     continue
 158  continue
      do 160 i=1,16
         gimp=zero
         do 164 k=1,3
            gimp=gimp+tr(k,i)*tge(k+16)
 164     continue
         trhse(i)=tge(i)+gimp
         do 165 j=1,16
            tcd1=zero
            tcd2=zero
            tcd3=zero
            do 161 k=1,3
               tcd1=tcd1+tr(k,i)*tke(16+k,j)
 161        continue
            do 162 k=1,3
               tcd2=tcd2+tke(i,k+16)*tr(k,j)
 162        continue
            do 163 k=1,3
               tcd3=tcd3+tr(k,i)*rw(k,j)
 163        continue
         tkele(i,j)=tke(i,j)+tcd1+tcd2+tcd3
 165  continue
 160  continue
!C
!C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
!C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
!C  WITH number(nod2) > number(nod1)
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         do 200 i=1,16
            tkele(i,13)=-tkele(i,13)
            tkele(13,i)=-tkele(13,i)
 200     continue
            trhse(13)=-trhse(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         do 210 i=1,16
            tkele(i,14)=-tkele(i,14)
            tkele(14,i)=-tkele(14,i)
 210     continue
            trhse(14)=-trhse(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
         do 220 i=1,16
            tkele(i,15)=-tkele(i,15)
            tkele(15,i)=-tkele(15,i)
 220     continue
            trhse(15)=-trhse(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
         do 230 i=1,16
            tkele(i,16)=-tkele(i,16)
            tkele(16,i)=-tkele(16,i)
 230     continue
            trhse(16)=-trhse(16)
      endif
      return
      end subroutine

! Procedure 81
! ------------



      subroutine cksel2(isub,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,kindt, &
                       kdata,kelos,tdata,tshag,wg,ipr,derx1,dery1,   &
                   derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2)
!C
!C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension tkse(10,10),tgse(10),kindt(nelt),kdata(ndata),  &
               kelos(ndata,2),tdata(ndata,4),tshag(10,8,ng),    &
               tjaci(2,2),t2j(3,3),tp(10),wg(ng),derx1(10),derx2(10),&
               dery1(10),dery2(10),tshagn(10,8,4),tjac(2,2),          &
               tr(10,10),ttemp(10,10),gtemp(10),ep(10)
      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      do 10 i=1,10
         tgse(i)=zero
         gtemp(i)=zero
         do 15 j=1,10
            tkse(i,j)=zero
            ttemp(i,j)=zero
            tr(i,j)=zero
 15      continue
         do 18 j=1,8
            do 19 k=1,4
               tshagn(i,j,k)=zero
 19         continue
 18      continue
 10   continue
      if(itcs.eq.1.or.itcs.eq.3) then
      u=(u0+u1+u2)/trois
      v=(v0+v1+v2)/trois
      endif
      rlrel=1
      if(itcs.eq.2.or.itcs.eq.3) then
      rlrel=(rll0+rll1+rll2)/trois
      rlrel=(1.D0/(rll0*rll0)+1.D0/(rll1*rll1)+1.D0/(rll2*rll2))/trois
      rlrel=1.D0/sqrt(rlrel)
      endif
!C
!C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
!C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CKSEL2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
!c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
!c         write(6,*) 'Detj now',detj
        detj=max(abs((x2-x1)*(y0-y1)),abs((x0-x1)*(y2-y1)))
        detj=max(detj,abs((x1-x0)*(y2-y0)),abs((x2-x0)*(y1-y0)))
        detj=max(detj,abs((x1-x2)*(y0-y2)),abs((x0-x2)*(y1-y2)))
        detj=max(detj,abs((x1-x2)*(x1-x2)),abs((x0-x2)*(x0-x2)))
        detj=max(detj,abs((y1-y2)*(y1-y2)),abs((y0-y2)*(y0-y2)))
        detj=max(detj,abs((x1-x0)*(x1-x0)),abs((y0-y1)*(y0-y1)))
        write(6,*) 'changed detj to ',detj
      endif
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      IF(IPR.GE.5) then
         WRITE (6,405) iel,isub
      endif
 405  FORMAT(///,T10,'JACOBIAN MATRIX FOR ELEMENT ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjac,2,2,2,6)
      IF(IPR.GE.5) then
         WRITE (6,406) iel,isub
      endif
 406  FORMAT(///,T10,'INVERSE JAC. MATRIX FOR EL. ',I5,'  SUB=',i5,///)
      IF(IPR.GE.5) CALL IMPMAT(tjaci,2,2,2,6)
!C
!C  For second derivatives: see Dhatt & Touzot (p57)
!C
      t2j(1,1)=tjaci(1,1)*tjaci(1,1)
      t2j(2,2)=tjaci(2,2)*tjaci(2,2)
      t2j(1,2)=tjaci(1,2)*tjaci(1,2)
      t2j(2,1)=tjaci(2,1)*tjaci(2,1)
      t2j(3,1)=tjaci(1,1)*tjaci(2,1)
      t2j(3,2)=tjaci(1,2)*tjaci(2,2)
      t2j(1,3)=deux*tjaci(1,1)*tjaci(1,2)
      t2j(2,3)=deux*tjaci(2,1)*tjaci(2,2)
      t2j(3,3)=tjaci(1,1)*tjaci(2,2)+tjaci(1,2)*tjaci(2,1)
      IF(IPR.GE.6) then
         WRITE (6,46) iel,isub
      endif
 46   FORMAT(///,T10,'   T2   MATRIX FOR ELEMENT ',I5,'  SUB=',i5,///)
      IF(IPR.GE.6) CALL IMPMAT(t2j,3,3,3,6)
!C
!C  TRANSFORMATION OF SHAPE FUNCTIONS DERIVATIVES
!C
      do 20 ig=1,ng
         do 30 k=1,10
!C
!C  First Derivatives
!C
            tp(1)=tjaci(1,1)*tshag(k,2,ig)+tjaci(1,2)*tshag(k,3,ig)
            tp(2)=tjaci(2,1)*tshag(k,2,ig)+tjaci(2,2)*tshag(k,3,ig)
            tshagn(k,2,ig)=tp(1)
            tshagn(k,3,ig)=tp(2)
!C
!C  Second Derivatives
!C
            tp(1)=t2j(1,1)*tshag(k,4,ig)+t2j(1,2)*tshag(k,5,ig)+t2j(1,3)*tshag(k,6,ig)
            tp(2)=t2j(2,1)*tshag(k,4,ig)+t2j(2,2)*tshag(k,5,ig)+t2j(2,3)*tshag(k,6,ig)
            tp(3)=t2j(3,1)*tshag(k,4,ig)+t2j(3,2)*tshag(k,5,ig)+t2j(3,3)*tshag(k,6,ig)
            tshagn(k,4,ig)=tp(1)
            tshagn(k,5,ig)=tp(2)
            tshagn(k,6,ig)=tp(3)
 30      continue
 20   continue
!C
!C  SUB-ELEMENTARY MATRIX FOR NORM SPLINE PROBLEM
!C
      do 100 i=1,10
         do 110 j=1,10
            do 120 ig=1,ng
!C
!C  Second derivative terms
!C
              tkse(i,j)=tkse(i,j)+wg(ig)*(tshagn(i,4,ig)*tshagn(j,4,ig)+tshagn(i,5,ig)*tshagn(j,5,ig) &
                                   +deux*tshagn(i,6,ig)*tshagn(j,6,ig))
!C
!C  First derivative terms
!C
              tkse(i,j)=tkse(i,j)+wg(ig)*alpha1/RLREL/RLREL*(tshagn(i,2,ig)*tshagn(j,2,ig)+tshagn(i,3,ig)*tshagn(j,3,ig))
!C
!C  Non derivated term
!C
              tkse(i,j)=tkse(i,j)+wg(ig)*alpha0/(RLREL*RLREL*RLREL*RLREL)*(tshag(i,1,ig)*tshag(j,1,ig))
!C
!C  If advection constraint
!C
              if(itcs.eq.1.or.itcs.eq.3) then
                 tkse(i,j)=tkse(i,j)+wg(ig)*wc1&
                        /(RLREL*RLREL)*       &
                        (                     &
                          (u*tshagn(i,2,ig)+v*tshagn(i,3,ig)-&
                           visc*tshagn(i,4,ig)-visc*tshagn(i,5,ig))*&
                          (u*tshagn(j,2,ig)+v*tshagn(j,3,ig)-        &
                           visc*tshagn(j,4,ig)-visc*tshagn(j,5,ig)))
              endif
 120       continue
           tkse(i,j)=tkse(i,j)*detj*stiff
 110    continue
 100  continue
!C
!C  CONTRIBUTION OF THE DATA POINTS
!C
      ifirst=kindt(iel)+1
      if(iel.eq.nelt) then
         ilast=ndatl
                      else
         ilast=kindt(iel+1)
      endif
      do 150 id=ifirst,ilast
         idata=kdata(id)
         if(kelos(idata,1).ne.iel) then
            write(6,*)' %%% ERROR - cksel2 : CHECK DATA SORTING %%% '
            stop
         endif
         isel=kelos(idata,2)
         if(isel.ne.isub) goto 150
         xo=tdata(idata,1)
         yo=tdata(idata,2)
         do=tdata(idata,3)
!CJMBJMB big test
         tdata(idata,4)=tdata(idata,4)/RLREL/RLREL
         wo=tdata(idata,4)
         if(ipr.ge.3) then
             write(6,151)xo,yo,do,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';xo=',f8.2,';do=',f8.2,';wo=',f8.2)
!C
!C Transformation of the data position in reference element
!C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
!C
!C  CONTRIBUTION FROM OBSERVATIONS
!C
         do 160 i=1,10
            tgse(i)=tgse(i)+wo*do*ep(i)*stiff*stiff
!c     &                       /(RLREL*RLREL)
            do 170 j=1,10
               tkse(i,j)=tkse(i,j)+wo*ep(i)*ep(j)*stiff*stiff
!c     &                       /(RLREL*RLREL)
 170        continue
 160     continue
 150  continue
      IF(IPR.GE.6) then
         WRITE (6,407) iel,isub
      endif
 407  FORMAT(///,T10,'SUB-ELEMENT PRE-MATRIX FOR EL ',I5,', SUB',I5,///)
      IF(IPR.GE.6) CALL IMPMAT(tkse,10,10,10,6)
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
!C
!C  CALCULATION OF DERIVATIVES AT INTERIOR INTERFACES (FOR SUBSEQUENT
!C  IDENTIFICATION OF NORMAL DERIVATIVES, in ckele2)
!C
      do 300 k=1,10
         tp(1)=tjaci(1,1)*tshag(k,7,1)+tjaci(1,2)*tshag(k,8,1)
         tp(2)=tjaci(2,1)*tshag(k,7,1)+tjaci(2,2)*tshag(k,8,1)
         derx1(k)=tp(1)
         dery1(k)=tp(2)
 300  continue
      do 310 k=1,10
         tp(1)=tjaci(1,1)*tshag(k,7,2)+tjaci(1,2)*tshag(k,8,2)
         tp(2)=tjaci(2,1)*tshag(k,7,2)+tjaci(2,2)*tshag(k,8,2)
         derx2(k)=tp(1)
         dery2(k)=tp(2)
 310  continue
!C
!C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
!C  TR IS THE TRANSFORMATION MATRIX
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 400 k=1,3
         is=(k-1)*3+1
         tr(is,is)=un
         do 410 i=1,2
            do 420 j=1,2
               tr(is+i,is+j)=tjac(i,j)
 420        continue
 410     continue
 400  continue
      tr(10,10)=p1
      tr(10,4)=-trois*p2/(deux*dist12)
      tr(10,7)=-tr(10,4)
      tr(10,5)=p2*(x1-x2)/(quatre*dist12)
      tr(10,8)=tr(10,5)
      tr(10,6)=p2*(y1-y2)/(quatre*dist12)
      tr(10,9)=tr(10,6)
      IF(IPR.GE.6) then
         WRITE (6,707) iel,isub
      endif
 707  FORMAT(///,T10,'TRANSFORM. MATRIX FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.6) CALL IMPMAT(tr,10,10,10,6)
!C
!C TRANSFORMATION OF TKSE
!C
      do 450 i=1,10
         do 460 j=1,10
            do 470 k=1,10
               ttemp(i,j)=ttemp(i,j)+tkse(i,k)*tr(k,j)
 470        continue
 460     continue
 450  continue
      do 550 i=1,10
         do 560 j=1,10
            tkse(i,j)=zero
            do 570 k=1,10
               tkse(i,j)=tkse(i,j)+tr(k,i)*ttemp(k,j)
 570        continue
 560     continue
 550  continue
!C
!C TRANSFORMATION OF TGSE
!C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue
!C
!C TRANSFORMATION OF DERX1
!C
      do 750 i=1,10
         gtemp(i)=zero
         do 760 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*derx1(k)
 760     continue
 750  continue
      do 770 i=1,10
         derx1(i)=gtemp(i)
 770  continue
!C
!C TRANSFORMATION OF DERX2
!C
      do 850 i=1,10
         gtemp(i)=zero
         do 860 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*derx2(k)
 860     continue
 850  continue
      do 870 i=1,10
         derx2(i)=gtemp(i)
 870  continue
!C
!C TRANSFORMATION OF DERY1
!C
      do 950 i=1,10
         gtemp(i)=zero
         do 960 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*dery1(k)
 960     continue
 950  continue
      do 970 i=1,10
         dery1(i)=gtemp(i)
 970  continue
!C
!C TRANSFORMATION OF DERY2
!C
      do 1050 i=1,10
         gtemp(i)=zero
         do 1060 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*dery2(k)
 1060    continue
 1050 continue
      do 1070 i=1,10
         dery2(i)=gtemp(i)
 1070 continue
      IF(IPR.GE.5) then
         WRITE (6,507) iel,isub
      endif
 507  FORMAT(///,T10,'SUB-ELEMENT MATRIX FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.5) CALL IMPMAT(tkse,10,10,10,6)
      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end subroutine

! Procedure 82
! ------------


      subroutine fixbcd(tuppe,tlowe,tdiag,trhsg,klink,kskyh,tcndi,kcndi)
!C
!C  FIX BOUNDARY CONDITIONS IN THE GLOBAL MATRIX SYSTEM
!C
      include'divapre.h'
      include'divainc.h'
      dimension tuppe(nterm),tlowe(*),tdiag(nddlt),trhsg(nddlt),tcndi(ncond),kcndi(ncond,info),kskyh(nddlt+1),klink(nnt)
      un=1.D0
      zero=0.D0
      do 10 i=1,ncond
         inod=kcndi(i,1)
         iddl=klink(inod)
         if(ityp.eq.2.or.ityp.eq.3) then
            iddl=klink(inod)+kcndi(i,2)-1
         endif
         tdiag(iddl)=un
         trhsg(iddl)=tcndi(i)
         imin=kskyh(iddl)
         imax=kskyh(iddl+1)-1
         do 20 ip=imin,imax
            ig=iddl-(imax-ip)-1
            trhsg(ig)=trhsg(ig)-tuppe(ip)*tcndi(i)
            tuppe(ip)=zero
            if(isym.ne.0) tlowe(ip)=zero
 20      continue
         do 30 j=iddl+1,nddlt
            hj=kskyh(j+1)-kskyh(j)
            imin=j-hj
            if(imin.gt.iddl) goto 30
            i1=kskyh(j+1)-j+iddl
            if(isym.ne.0) then
               trhsg(j)=trhsg(j)-tlowe(i1)*tcndi(i)
               tlowe(i1)=zero
               tuppe(i1)=zero
                          else
               trhsg(j)=trhsg(j)-tuppe(i1)*tcndi(i)
               tuppe(i1)=zero
            endif
 30      continue
 10   continue
      return
      end subroutine

! Procedure 83
! ------------



      subroutine impsol(tcoog,klink,sol)
!C
!C  PRINT THE SOLUTION AT PRINCIPAL CONNECTORS
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),klink(nnt),sol(nddlt)
      if(ityp.eq.2.or.ityp.eq.3) then
!c         write(81,400)
         do 10 i=1,nnt1
            x=tcoog(i,1)
            y=tcoog(i,2)
            val=sol(klink(i))
            valx=sol(klink(i)+1)
            valy=sol(klink(i)+2)
            write(81,401)x,y,val,valx,valy
 10      continue
      endif
 400  format(72('&'),/,t8,'X',t18,'Y',t29,'PHI',t44,'PHIX',t59,'PHIY',/,72('&'))
 401  format(t3,f8.3,t13,f8.3,t26,f10.5,t41,f10.5,t56,f10.5)
      return
      end subroutine


! Procedure 84
! ------------


      subroutine locse2 (klocs)
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension klocs(3,10)
      klocs(1,1)=13
      klocs(1,2)=14
      klocs(1,3)=15
      klocs(1,4)=1
      klocs(1,5)=2
      klocs(1,6)=3
      klocs(1,7)=4
      klocs(1,8)=5
      klocs(1,9)=6
      klocs(1,10)=10
      klocs(2,1)=13
      klocs(2,2)=14
      klocs(2,3)=15
      klocs(2,4)=4
      klocs(2,5)=5
      klocs(2,6)=6
      klocs(2,7)=7
      klocs(2,8)=8
      klocs(2,9)=9
      klocs(2,10)=11
      klocs(3,1)=13
      klocs(3,2)=14
      klocs(3,3)=15
      klocs(3,4)=7
      klocs(3,5)=8
      klocs(3,6)=9
      klocs(3,7)=1
      klocs(3,8)=2
      klocs(3,9)=3
      klocs(3,10)=12
      return
      end subroutine

! Procedure 85
! ------------


      subroutine locse3 (klocs)
!C
!C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
!C
      include'divapre.h'
      include'divainc.h'
      dimension klocs(4,10)
      klocs(1,1)=17
      klocs(1,2)=18
      klocs(1,3)=19
      klocs(1,4)=1
      klocs(1,5)=2
      klocs(1,6)=3
      klocs(1,7)=4
      klocs(1,8)=5
      klocs(1,9)=6
      klocs(1,10)=13
      klocs(2,1)=17
      klocs(2,2)=18
      klocs(2,3)=19
      klocs(2,4)=4
      klocs(2,5)=5
      klocs(2,6)=6
      klocs(2,7)=7
      klocs(2,8)=8
      klocs(2,9)=9
      klocs(2,10)=14
      klocs(3,1)=17
      klocs(3,2)=18
      klocs(3,3)=19
      klocs(3,4)=7
      klocs(3,5)=8
      klocs(3,6)=9
      klocs(3,7)=10
      klocs(3,8)=11
      klocs(3,9)=12
      klocs(3,10)=15
      klocs(4,1)=17
      klocs(4,2)=18
      klocs(4,3)=19
      klocs(4,4)=10
      klocs(4,5)=11
      klocs(4,6)=12
      klocs(4,7)=1
      klocs(4,8)=2
      klocs(4,9)=3
      klocs(4,10)=16
      return
      end subroutine
      

! Procedure 86
! ------------
      
      subroutine uur(iu,U,im,jm,valex,valrepl)
      real*4 U(im,jm)
      real*8 c8
      real*4 valex
      valex=0
      write(6,*) 'Going to read constraint file, size ', im,jm 
      call ureadc(iu,c8,U,valex,ipr,imax,jmax,kmax,lw)
      write(6,*) 'Finished: read constraint file, size ', imax,jmax
      if(im.ne.imax) write(6,*) 'Problem in constraint file',iu
      if(jm.ne.jmax) write(6,*) 'Problem in constraint file',iu
      
!C implement it with ureadc and check for dimensions be coherent with info file
!c      do i=1,im
!C      read(iu,*) (U(i,j),j=1,jm)
!C      enddo
      
      do i=1,im
       do j=1,jm
!c       if(abs(U(i,j)).gt.999.) U(i,j)=0
       if(U(i,j).eq.valex) U(i,j)=valrepl
  

       enddo
       enddo
      return
      end subroutine
      
! Procedure 87
! ------------
      
    
      subroutine JMBCONSTRL(U,X,Y,II,icord,valexll)
!C,dxkm,dykm)
      include 'divapre.h'
      include 'divainc.h'
      real*8 U,X,Y
      real*4 valexll

      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf,xt,yt,UT,VT

      integer II,imaxf,jmaxf,icord
      integer imaxff,jmaxff
!c      parameter(imaxff=500,jmaxff=500)
!c      real*4 UF(imaxff,jmaxff),VF(imaxff,jmaxff)
      real*8 xx,yy,dxkm,dykm
      COMMON/JMBCONL/x0f,dxxf,dyxf,y0f,dxyf,dyyf,UF,VF,imaxf,jmaxf,ierrl
!C23456
      U=1
      
      if(II.EQ.0) then
      
      
      ierrl=0
!C implement matrix reading
!C Use units 91 92 (U,V) and 93(grid info)
      read(95,*,end=922,err=922) x0f
      read(95,*,end=922,err=922) y0f
      read(95,*,end=922,err=922) dxxf
      read(95,*,end=922,err=922) dyyf
      read(95,*,end=922,err=922) imaxf
      read(95,*,end=922,err=922) jmaxf
      x0f=x0f-dxxf
      y0f=y0f-dyyf
!C
!C
!c      if(imaxf*jmaxf.gt.(imaxff*jmaxff)) then
!c      write(6,*) 'increase imaxff*jmaxff to', imaxf*jmaxf
!c      goto 922
!c      endif
!C Use allody and save pointer (to use allody with negative values afterwards?)
!C
      NSWSP=imaxf*jmaxf
!C Try to read in simple precision in two fields (eq 1 double precision)
      NSWSP=NSWSP/2+mod(NSWSP,2)
      call allody(NSWSP,1,'WORKS',IPWSP,1)
      vrepl=1
      call uur(94,S(IPWSP),imaxf,jmaxf,valexll,vrepl)
!C      call uur(92,S(IPWSP+NSWSP/2),imaxf,jmaxf)
!C      call uur(91,UF,imaxf,jmaxf)
!C      call uur(92,VF,imaxf,jmaxf)
      goto 923
 922  continue
      write(6,*) 'Error reading grid info for variable L '
      write(6,*) 'Assuming relative length scale=1'
      ierrl=1
      U=1
      return
 
!C replace exclusion values by zero
!C Need to change coordinates for x0 etc...

      write(6,*) 'Preparing constraint'
      write(6,*) 'Coordinate change',icord
!CJMB TEST
 
 923  continue
!C End TEST UF VF dyyf dxxf x0f y0f imaxff jmaxff = read in
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
      
!C
!C
      return
      endif
      if(II.EQ.1) then
!C
      if (ierrl.eq.1) then
      U=1
      return
      endif
      xt=X
      yt=Y
       
       call bilininl(S(IPWSP),x0f,dxxf,dyxf,y0f,dxyf,dyyf,imaxf,jmaxf,UT,xt,yt)
      U=UT
      
!C      write(6,*) 'X,Y',x,y,UT,VT
!C
      return
      endif
      end subroutine
      
! Procedure 88
! ------------
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  INTSEC (calculate center coordinates of quadrangular element)
!C     -  ISTRIA (check if one point lies in a triangle)
!C     -  PRSKYH (print a vector to check ...)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      subroutine intsec(x1,x2,x3,x4,y1,y2,y3,y4,x0,y0)
!C
!C  CALCULATE CENTER COORDINATES OF QUADRANGULAR ELEMENT
!C               (I.E., WHEN ITYP.EQ.3)
!C
      include'divapre.h'
      eps=0.0001
      un=1.d0
      dx=x3-x1
      dy=y3-y1
      if(abs(dx).gt.eps) then
         a1=dy/dx
         b1=y1-x1*dy/dx
         i1=1
                         else
         a1=dx/dy
         b1=x1-y1*dx/dy
         i1=-1
      endif
      dx=x4-x2
      dy=y4-y2
      if(abs(dx).gt.eps) then
         a2=dy/dx
         b2=y2-x2*dy/dx
         i2=1
                         else
         a2=dx/dy
         b2=x2-y2*dx/dy
         i2=-1
      endif
      if(i1.eq.1.and.i2.eq.1) then
         x0=(b2-b1)/(a1-a2)
         y0=a1*x0+b1
         return
      endif
      if(i1.eq.-1.and.i2.eq.-1) then
         y0=(b2-b1)/(a1-a2)
         x0=a1*y0+b1
         return
      endif
      if(i1.eq.1.and.i2.eq.-1) then
         y0=(a1*b2+b1)/(un-a1*a2)
         x0=a2*y0+b2
         return
      endif
      if(i1.eq.-1.and.i2.eq.1) then
         y0=(a2*b1+b2)/(un-a1*a2)
         x0=a1*y0+b1
         return
      endif
      end subroutine

! Procedure 89
! ------------

!C     SUBROUTINE istria
!C     =================
      subroutine istria(x,y,x1,y1,x2,y2,x3,y3,is)
!C
!C  CHECK IF (X,Y) IS IN THE TRIANGLE DEFINED BY THE THREE NODES
!C    (X1,Y1),(X2,Y2),(X3,Y3)
!C

      include'divapre.h'

      real*8 ksi,eta,dum,epsi
!c      write(6,*) 'el ',x1,y1,x2,y2,x3,y3
      dum=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
      ksi=((y3-y1)*(x-x1)-(x3-x1)*(y-y1))/dum
      eta=((x2-x1)*(y-y1)-(y2-y1)*(x-x1))/dum
      epsi=1.e-4
      is=0
      if (ksi.gt.-epsi.and.eta.gt.-epsi.and. &
         (ksi+eta).lt.(1.+epsi)) then
       if (-epsi.lt.eta.and. &
           -epsi+eta.lt.ksi.and. &
           eta.lt.0.5-0.5*ksi+epsi) then
        is=1
       elseif (-epsi.lt.ksi.and. &
           -epsi+ksi.lt.eta.and.  &
           eta.lt.1.-2.*ksi+epsi) then
        is=3
       elseif (-epsi+eta.lt.1+epsi.and. &
           eta.gt.0.5-0.5*ksi-epsi.and. &
           eta.gt.1.-2.*ksi-epsi) then
        is=2
       endif
      endif
      return

      end subroutine

! Procedure 90
! ------------

!C OBSOLETE
!C     SUBROUTINE istria
!C     =================

      subroutine istriaold(x,y,x1,y1,x2,y2,x3,y3,is)
!C
!C  CHECK IF (X,Y) IS IN THE TRIANGLE DEFINED BY THE THREE NODES
!C    (X1,Y1),(X2,Y2),(X3,Y3)
!C
      include'divapre.h'
      zero=0.D0
      is=0
      epsi=-1.e-1
!cmr      ri1=fst(x,y,x1,y1,x2,y2)*fst(x3,y3,x1,y1,x2,y2)
!cmr      ri2=fst(x,y,x2,y2,x3,y3)*fst(x1,y1,x2,y2,x3,y3)
!cmr      ri3=fst(x,y,x3,y3,x1,y1)*fst(x2,y2,x3,y3,x1,y1)
      ri1=fst(x,y,x1,y1,x2,y2)*fst(x3,y3,x1,y1,x2,y2)
      ri2=fst(x,y,x2,y2,x3,y3)*fst(x1,y1,x2,y2,x3,y3)
      ri3=fst(x,y,x3,y3,x1,y1)*fst(x2,y2,x3,y3,x1,y1)
!c      write(6,*) ri1,ri2,ri3
      if(ri1.ge.zero.and.ri2.ge.zero.and.ri3.ge.zero) is=1
      return
      end subroutine

! Procedure 91
! ------------

      function fst(x,y,x1,y1,x2,y2) result(value)
      include'divapre.h'
      real*8 value
      dx=x2-x1
!c      write(6,*) dx
      if(abs(dx).le.(0.0001)) then
         value=x-x1-(dx/(y2-y1))*(y-y1)
         return
      endif
      value=y-y1-((y2-y1)/dx)*(x-x1)
      return
      end function

! Procedure 92
! ------------


      SUBROUTINE PRSKYH(LVECT,LONG)
!C     ==========================
      include'divapre.h'
      DIMENSION LVECT(LONG)
      WRITE(6,101)
      IDEB=0
 5    WRITE(6,100)(LVECT(IDEB+I),I=1,10)
      IDEB=IDEB+10
      IF(IDEB.LT.LONG) GO TO 5
 100  FORMAT (T5,10(' ',I6))
 101  FORMAT('   CHECK KSKYH (OR OTHER ...) VECTOR',/,T4,33('='))
      RETURN
      END subroutine
      
! Procedure 93
! ------------


!C     -  APPEND (transform an elementary matrix in a vector)
!C     -  ASSEL  (assembling of elementary array accorting to skyline)
!C     -  CALLOC (compute the localization of one element in the struct.)
!C     -  CALSKY (compute the KSKYH vector from the topologic data)
!C     -  IMPMAT (print a matrix)
!C     -  LOCPT2 (locate the (x,y) point in the structure (for ityp=2)
!C     -  LOCPT3 (locate the (x,y) point in the structure (for ityp=3)
!C     -  SOL    (solution of a linear system according to skyline meth.)
!C     -  UWRITC (write a matrix in standard GHER format)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




      subroutine append(tkele,nddle,tvele,nspace,isym)
      include'divapre.h'
      dimension tkele(nddle,nddle),tvele(nspace)
!C
!C  TRANSFORM AN ELEMENTARY MATRIX IN A VECTOR
!C  IF ISYM = 1 ==> ONLY THE UPPER PART OF THE MATRIX
!C
      iv=1
      if(isym.eq.0) then
         do 10 j=1,nddle
            do 20 i=1,j
               tvele(iv)=tkele(i,j)
               iv=iv+1
 20         continue
 10      continue
                    else
         do 30 j=1,nddle
            do 40 i=1,nddle
               tvele(iv)=tkele(i,j)
               iv=iv+1
 40         continue
 30      continue
      endif
      return
      end subroutine

! Procedure 94
! ------------


      SUBROUTINE ASSEL(IKG,IFG,IDLE,NSYM,KLOCE,KLD,VKE,VFE,VKGS,VKGD,VKGI,VFG)
!C=======================================================================
!C     ASSEMBLAGE D'UNE MATRICE ET/OU D'UN VECTEUR ELEMENTAIRE
!C     (MATRICE SYMETRIQUE OU NON)
!C       ENTREES
!C            AUTEURS: Dhatt et Touzot, 1985
!C          IKG    SI IKG.EQ.1 ASSEMBLAGE DE LA MATRICE ELEMENTAIRE KE
!C          IFG    SI IFG.EQ.1 ASSEMBLAGE DU VECTEUR ELEMENTAIRE FE
!C          IDLE   NOMBRE DE D. L. DE L'ELEMENT
!C          NSYM   0=PROBLEME SYMETRIQUE, 1=PROBLEME NON SYMETRIQUE
!C          KLOCE  VECTEUR DE LOCALISATION DE L'ELEMENT
!C          KLD    HAUTEURS CUMULEES DE COLONNES DE KG
!C          VKE    MATRICE ELEMENTAIRE KE(PLEINE OU TRIANGLE SUPERIEUR
!C                 PAR COLONNES DESCENDANTES)
!C          VFE    VECTEUR ELEMENTAIRE FE
!C       SORTIES
!C          VKGS,VKGD,VKGI   MATRICE GLOBALE (LIGNE DE CIEL)
!C                  (SYMETRIQUE OU NON)
!C          VFG    VECTEUR SOLLICITATIONS GLOBAL
!C=======================================================================
      include'divapre.h'
      DIMENSION KLOCE(*),KLD(*),VKE(*),VFE(*),VKGS(*),VKGD(*), VKGI(*),VFG(*)
!C.......................................................................
!C
!C......   ASSEMBLAGE DE LA MATRICE ELEMENTAIRE
!C
      IF(IKG.NE.1) GO TO 100
      IEQ0=IDLE
      IEQ1=1
!C......   POUR CHAQUE COLONNE DE KE
      DO 90 JD=1,IDLE
      IF(NSYM.NE.1) IEQ0=JD
      JL=KLOCE(JD)
!C      IF(JL)90,90,10
!C replaced old fashioned if
      IF (JL.LE.0) goto 90
 10   I0=KLD(JL+1)
      IEQ=IEQ1
      IQ=1
!C......   POUR CHAQUE LIGNE DE KE
      DO 80 ID=1,IDLE
      IL=KLOCE(ID)
      IF(NSYM.EQ.1) GO TO 30
!C      IF(ID-JD) 30,20,20
!C replaced old fashioned if test
      IF(ID.LT.JD) goto 30
 20   IQ=ID
!C 30   IF(IL) 80,80,40
!C replaced old fashioned if test
  30   IF(IL.LE.0) goto 80
 40   IJ=JL-IL
!C      IF(IJ) 70,50,60
!C replaced old fashioned if test
       IF(IJ.LT.0) goto 70
       IF(IJ.GT.0) goto 60
!C......   TERMES DIAGONAUX DE KG
 50   VKGD(IL)=VKGD(IL)+VKE(IEQ)
      GO TO 80
!C......   TERMES DU TRIANGLE SUPERIEUR DE KG
 60   I=I0-IJ
      VKGS(I)=VKGS(I)+VKE(IEQ)
      GO TO 80
!C......   TERMES DU TRIANGLE INFERIEUR DE KG
 70   IF(NSYM.NE.1) GO TO 80
      I=KLD(IL+1)+IJ
      VKGI(I)=VKGI(I)+VKE(IEQ)
 80   IEQ=IEQ+IQ
 90   IEQ1=IEQ1+IEQ0
!C
!C......   ASSEMBLAGE DU VECTEUR ELEMENTAIRE
!C
 100  IF(IFG.NE.1) GO TO 130
      DO 120 ID=1,IDLE
      IL=KLOCE(ID)
!C      IF(IL) 120,120,110
!C replaced old fashioned if test
      IF(IL.LE.0) goto 120
 110  VFG(IL)=VFG(IL)+VFE(ID)
 120  CONTINUE
 130  RETURN
      END subroutine

! Procedure 95
! ------------


      subroutine calloc(iel,loce,nloc,kconn,klink,ipr)

!C  COMPUTE THE LOCALISATION OF THE D.O.F. OF ELEMENT IEL IN THE
!C  GLOBAL D.O.F. VECTOR

      include'divapre.h'
      include'divainc.h'
      dimension loce(nloc),kconn(nelt,nnel),klink(nnt)
      do 10 i=1,nloc
         loce(i)=0
 10   continue
!C
!C  COMPUTE LOCE FOR ELEMENTS OF TYPE 2 : 3 * 3 DOF AT VERTEX NODES
!C                                        + 3 INTERFACES
!C
      if(ityp.eq.2) then
         inod1=kconn(iel,1)
         inod2=kconn(iel,3)
         inod3=kconn(iel,5)
         iint1=-kconn(iel,2)
         iint2=-kconn(iel,4)
         iint3=-kconn(iel,6)
         loce(1)=klink(inod1)
         loce(2)=loce(1)+1
         loce(3)=loce(2)+1
         loce(4)=klink(inod2)
         loce(5)=loce(4)+1
         loce(6)=loce(5)+1
         loce(7)=klink(inod3)
         loce(8)=loce(7)+1
         loce(9)=loce(8)+1
         loce(10)=klink(iint1)
         loce(11)=klink(iint2)
         loce(12)=klink(iint3)
      endif
!C
!C  COMPUTE LOCE FOR ELEMENTS OF TYPE 3 : 4 * 3 DOF AT VERTEX NODES
!C                                        + 4 INTERFACES
!C
      if(ityp.eq.3) then
         inod1=kconn(iel,1)
         inod2=kconn(iel,3)
         inod3=kconn(iel,5)
         inod4=kconn(iel,7)
         iint1=-kconn(iel,2)
         iint2=-kconn(iel,4)
         iint3=-kconn(iel,6)
         iint4=-kconn(iel,8)
         loce(1)=klink(inod1)
         loce(2)=loce(1)+1
         loce(3)=loce(2)+1
         loce(4)=klink(inod2)
         loce(5)=loce(4)+1
         loce(6)=loce(5)+1
         loce(7)=klink(inod3)
         loce(8)=loce(7)+1
         loce(9)=loce(8)+1
         loce(10)=klink(inod4)
         loce(11)=loce(10)+1
         loce(12)=loce(11)+1
         loce(13)=klink(iint1)
         loce(14)=klink(iint2)
         loce(15)=klink(iint3)
         loce(16)=klink(iint4)
      endif
!C
!C  PRINT LOCE FOR ELEMENT IEL ...
!C
      if(ipr.ge.5) then
        write(6,*)'  LOCE COMPUTED FOR ELEMENT ',IEL
        write(6,910) (loce(i),i=1,nddle)
 910    format(12(i6))
      endif
      return
      end subroutine

! Procedure 96
! ------------


      subroutine calsky(kskyh,kconn,klink,kloce,ipr)
!C
!C  COMPUTE THE KSKYH VECTOR FROM THE TOPOLOGIC DATA
!C
      include'divapre.h'
      include'divainc.h'
      dimension kskyh(nddlt+1),kconn(nelt,nnel),klink(nnt),kloce(nddle)
      do 10 iel=1,nelt
         call calloc(iel,kloce,nddle,kconn,klink,ipr)
         lhmin=999999
         do 20 id=1,nddle
            lhmin=min(lhmin,kloce(id))
 20      continue
         do 30 id=1,nddle
            iloc=kloce(id)
            lh=iloc-lhmin
            if(kskyh(iloc).lt.lh) kskyh(iloc)=lh
 30      continue
 10   continue
      if(ipr.ge.5) call prskyh(kskyh,nddlt+1)
!C
!C  compute the cumulated height index vector
!C
      kskyh(1)=1
      inter1=kskyh(2)
      kskyh(2)=1
      do 50 i=3,nddlt+1
         inter2=kskyh(i)
         kskyh(i)=kskyh(i-1)+inter1
         inter1=inter2
 50   continue
!C
!C  INITIALIZATION OF NTERM (SIZE OF NON DIAG VECTOR CONTAINING THE
!C  UPPER OR LOWER STIFFNESS MATRIX
!C
      nterm=kskyh(nddlt+1)-1
      if(ipr.ge.5) call prskyh(kskyh,nddlt+1)
      return
      end subroutine
      
! Procedure 97
! ------------
      

      SUBROUTINE IMPMAT(A,L,M,NDIM,IUCT)
!C     ==================================
      include'divapre.h'
      include'../../Mesh/iodv.h'
      DIMENSION A(NDIM,M)
      if (iodv.eq.0) then

      K=1
 1    KK=K+7
!C      IF (KK-M) 3,3,2
      if(KK.LE.M) goto 3
 2    KK=M
 3    WRITE(IUCT,200) (J,J=K,KK)
      DO 4 I=1,L
 4    WRITE(IUCT,201) I,(A(I,J),J=K,KK)
      K=K+8
!C      IF (K-M) 1,1,5
      if(K.LE.M) goto 1
      if(K.GT.M) goto 5
                     else
      DO 10 J=1,M
      DO 10 I=1,L
      WRITE(IUCT,*) A(I,J)
 10   continue 
      endif


 5    RETURN
 200  FORMAT('0',8(9X,I3,3X))
 201  FORMAT(1X,I3,8(E15.5))
      END subroutine
      
! Procedure 98
! ------------
      


      subroutine locpt2(x,y,tcoog,kconn,ielem,isub,ipr)
!C
!C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 2)
!C
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2)
      ielem=-1
      isub=-1
      iel=0
 100  iel=iel+1
      if(iel.gt.nelt) goto 20
!cmr
!c      write(34,*) x,y,iel
!C
!C  DOES DATA ID BELONG TO ELEMENT IEL ?
!C
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 100
       isub=itria
       itria=1
!C
!C  WHICH SUB-ELEMENT IN ELEMENT IEL ?
!C
      ielem=iel
!c      iF(itria.eq.0) then
!c         write(6,*) '%%% ERROR  LOCPT2 IN THE LOCALIZATION %%%'
!c         stop
!c      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',i4,'(',i1,')')
 20   continue
      return
      end subroutine

! Procedure 99
! ------------

      subroutine locpt3(x,y,tcoog,kconn,tcele,ielem,isub,ipr)
!C
!C  LOCATE THE (X,Y) POINT IN THE F.E. STRUCTURE (for ITYP = 3)
!C
      include'divapre.h'
      include'divainc.h'
      dimension kconn(nelt,nnel),tcoog(nnt1,2),tcele(nelt,2)
      ielem=-1
      isub=-1
      iel=0
 100  iel=iel+1
      if(iel.gt.nelt) goto 20
!C
!C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 1 or 2)?
!C
      x1=tcoog(kconn(iel,1),1)
      y1=tcoog(kconn(iel,1),2)
      x2=tcoog(kconn(iel,3),1)
      y2=tcoog(kconn(iel,3),2)
      x3=tcoog(kconn(iel,5),1)
      y3=tcoog(kconn(iel,5),2)
      x4=tcoog(kconn(iel,7),1)
      y4=tcoog(kconn(iel,7),2)
      x0=tcele(iel,1)
      y0=tcele(iel,2)
      call istria(x,y,x1,y1,x2,y2,x3,y3,itria)
      if(itria.eq.0) goto 50
!C
!C  WHICH SUB-ELEMENT IN ELEMENT IEL (1 or 2 ) ?
!C
      ielem=iel
      call istria(x,y,x1,y1,x2,y2,x0,y0,itria)
      if(itria.ge.1) then
         isub=1
                     else
         isub=2
      endif
      goto 22
 50   continue
!C
!C  DOES DATA ID BELONG TO ELEMENT IEL (SUB-EL 3 or 4)?
!C
      call istria(x,y,x1,y1,x3,y3,x4,y4,itria)
      if(itria.eq.0) goto 100
!C
!C  WHICH SUB-ELEMENT IN ELEMENT IEL (3 or 4 ) ?
!C
      ielem=iel
      call istria(x,y,x1,y1,x0,y0,x4,y4,itria)
      if(itria.ge.1) then
         isub=4
                     else
         isub=3
      endif
 22   continue
      if(ipr.gt.2) write(6,21) x,y,ielem,isub
 21   format(t2,'    Locating point (',f7.1,',',f7.1,') in element ',i4,'(',i1,')')
 20   continue
      return
      end subroutine
      
! Procedure 100
! ------------
      


      SUBROUTINE SOL(VKGS,VKGD,VKGI,VFG,KLD,NEQ,MP,IFAC,ISOL,NSYM,ENERG)
!C=======================================================================
!C     RESOLUTION D'UN SYSTEME LINEAIRE SYMETRIQUE OU NON. LA MATRICE EST
!C     STOCKEE PAR LIGNE DE CIEL, EN MEMOIRE DANS LES TABLES
!C     VKGS,VKGD,VKGI
!C                    AUTEURS: Dhatt et Touzot, 1985
!C       ENTREES
!C          VKGS,VKGD,VKGI    MATRICE DU SYSTEME : PARTIES SUPERIEURE,
!C                            DIAGONALE, INFERIEURE
!C          VFG               SECOND MEMBRE
!C          KLD               POINTEURS VERS LES HAUTS DE COLONNE
!C          NEQ               NOMBRE D'EQUATIONS
!C          MP                UNITE LOGIQUE D'IMPRESSION
!C          IFAC              SI IFAC.EQ.1 TRIANGULARISATION DE
!C                            LA MATRICE
!C          ISOL              SI ISOL.EQ.1 CALCUL DE LA SOLUTION A
!C                            PARTIR DE LA MATRICE TRIANGULARISEE
!C          NSYM              INDICE DE PROBLEME NON SYMETRIQUE
!C       SORTIES
!C          VKGS,VKGD,VKGI    MATRICE TRIANGULARISEE
!C          VFG               SOLUTION (SI ISOL.EQ.1)
!C          ENERG             ENERGIE DU SYSTEME (SI NSYM.EQ.0)
!C=======================================================================
      include'divapre.h'
      DIMENSION VKGS(*),VKGI(*),VKGD(*),VFG(*),KLD(*)
      DATA ZERO/0.0D0/
!C.......................................................................
!c      write(6,*) 'into sol',neq,isol,nsym,ifac
      IK=1
      IF(VKGD(1).NE.ZERO) GO TO 10
      WRITE(MP,2000) IK
      STOP
 10   ENERG=ZERO
!C
!C.....  POUR CHAQUE COLONNE IK A MODIFIER
!C
      JHK=1
!C      write(6,*) 'into sol',JHK1
!C?????? JHK1 not defined????
!C      JHK1=0
!C??????
      
      DO 100 IK=2,NEQ
!C.....  POINTEUR DU HAUT DE LA COLONNE SUIVANTE IK+1
      JHK1=KLD(IK+1)
!C.....  HAUTEUR DE LA COLONNE IK (HORS TERMES SUPERIEUR ET DIAGONAL)
      LHK=JHK1-JHK
      LHK1=LHK-1
!C.....  LIGNE DU PREMIER TERME A MODIFIER DANS LA COLONNE IK
      IMIN=IK-LHK1
      IMIN1=IMIN-1
!C.......  LIGNE DU DERNIER TERME A MODIFIER DANS LA COLONNE IK
      IMAX=IK-1
      IF(LHK1.LT.0) GO TO 100
      IF(IFAC.NE.1) GO TO 90
      IF(NSYM.EQ.1) VKGI(JHK)=VKGI(JHK)/VKGD(IMIN1)
      IF(LHK1.EQ.0) GO TO 40
!C
!C.....  MODIFIER LES TERMES NON DIAGONAUX DE LA COLONNE IK
!C
      JCK=JHK+1
      JHJ=KLD(IMIN)
!C.....  POUR CHAQUE TERME PLACE EN JCK, CORRESPONDANT A LA COLONNE IJ
      DO 30 IJ=IMIN,IMAX
      JHJ1=KLD(IJ+1)
!C.....  NOMBRE DE TERMES MODIFICATIFS DU TERME PLACE EN JCK
      IC=MIN0(JCK-JHK,JHJ1-JHJ)
      IF(IC.LE.0.AND.NSYM.EQ.0) GO TO 20
      C1=ZERO
      IF(IC.LE.0) GO TO 17
      J1=JHJ1-IC
      J2=JCK-IC
      IF(NSYM.EQ.1) GO TO 15
      VKGS(JCK)=VKGS(JCK)-SCAL(VKGS(J1),VKGS(J2),IC)
      GO TO 20
 15   VKGS(JCK)=VKGS(JCK)-SCAL(VKGI(J1),VKGS(J2),IC)
      C1=SCAL(VKGS(J1),VKGI(J2),IC)
 17   VKGI(JCK)=(VKGI(JCK)-C1)/VKGD(IJ)
 20   JCK=JCK+1
 30   JHJ=JHJ1
!C
!C.....  MODIFIER LE TERME DIAGONAL
!C
 40   JCK=JHK
      CDIAG=ZERO
      DO 70 IJ=IMIN1,IMAX
      C1=VKGS(JCK)
      IF(NSYM.EQ.1) GO TO 50
      C2=C1/VKGD(IJ)
      VKGS(JCK)=C2
      GO TO 60
 50   C2=VKGI(JCK)
 60   CDIAG=CDIAG+C1*C2
 70   JCK=JCK+1
      VKGD(IK)=VKGD(IK)-CDIAG
!C      IF(VKGD(IK)) 90,80,90
      IF(VKGD(IK).LT.0) goto 90
      IF(VKGD(IK).GT.0) goto 90
 80   WRITE(MP,2000) IK
 2000 FORMAT(' *** ERREUR, PIVOT NUL EQUATION ',I5)
      STOP
!C
!C..... RESOLUTION DU SYSTEME TRIANGULAIRE INFERIEUR
!C
 90   IF(ISOL.NE.1) GO TO 100
!c      write(6,*) 'for sol',ik,jhk,imin1,lhk
      IF(NSYM.NE.1) VFG(IK)=VFG(IK)-SCAL(VKGS(JHK),VFG(IMIN1),LHK)
      IF(NSYM.EQ.1) VFG(IK)=VFG(IK)-SCAL(VKGI(JHK),VFG(IMIN1),LHK)
!c      write(6,*) 'endsol'
 100  JHK=JHK1
!C      write(6,*) 'JHK1',JHK1
      IF(ISOL.NE.1) RETURN
!C
!C.....  RESOLUTION DU SYSTEME DIAGONAL
!C
      IF(NSYM.EQ.1) GO TO 120
      DO 110 IK=1,NEQ
      C1=VKGD(IK)
      C2=VFG(IK)/C1
      VFG(IK)=C2
 110  ENERG=ENERG+C1*C2*C2
!C
!C.....  RESOLUTION DU SYSTEME TRIANGULAIRE SUPERIEUR
!C
 120  IK=NEQ+1
      JHK1=KLD(IK)
 130  IK=IK-1
      IF(NSYM.EQ.1) VFG(IK)=VFG(IK)/VKGD(IK)
      IF(IK.EQ.1) RETURN
      C1=VFG(IK)
      JHK=KLD(IK)
      JBK=JHK1-1
      IF(JHK.GT.JBK) GO TO 150
      IJ=IK-JBK+JHK-1
      DO 140 JCK=JHK,JBK
      VFG(IJ)=VFG(IJ)-VKGS(JCK)*C1
 140  IJ=IJ+1
 150  JHK1=JHK
      GO TO 130
      END subroutine

! Procedure 101
! ------------


      FUNCTION SCAL(X,Y,N) result(value)
!C=======================================================================
!C     PRODUIT SCALAIRE DES VECTEURS X ET Y DE LONGUEUR N
!C=======================================================================

      include'divapre.h'
      REAL*8 SUM,value
      DIMENSION X(*),Y(*)
      DATA ZERO/0.0D0/
!C.......................................................................
      value=ZERO
      SUM=0
!c      write(6,*) 'Prod SCAL',N

!$OMP  PARALLEL DO
!$OMP& REDUCTION(+:SUM)
      DO  I=1,N
       SUM=SUM+(X(I)*Y(I))
      ENDDO
!$OMP  END PARALLEL DO
      value=SUM
      RETURN
      END function

! Procedure 102
! ------------

	 subroutine uwbimg4(iu,c4,nx,ny,nz,spval,dx,dy,xori,yori)
	 implicit none


	 integer nx,ny,nz,nt,ndim,icod
	 integer i,j,k,l,iu
	 real*4 c4(nx,ny,nz)
	 real*4 spval,dx,dy,xori,yori
	 character*80 record

         nt=1
	 ndim=1 
	 icod=1

	 write(iu) record
	 write(iu) record
	 write(iu) record
	 write(iu) record

	 xori=xori+dx
         yori=yori+dy

	 write(iu) nx,ny,nz,nt,ndim,icod
	 write(iu) xori,yori,dx,dy,spval
	 write(iu)(float(k),k=1,nz)

	 do l=1,nt
	   write(iu) float(l)
	   do k=1,nz
	      write(iu) ((c4(i,j,k),i=1,nx),j=1,ny)
           enddo
	 enddo
 
	 return
	 end subroutine

! Procedure 103
! ------------



	 subroutine uwbimg8(iu,c8,nx,ny,nz,spvald,dxd,dyd,xorid,yorid)
	 implicit none

	 integer nx,ny,nz,nt,ndim,icod
	 integer i,j,k,l,iu
	 real*8 c8(nx,ny,nz)
	 real*8 spvald,dxd,dyd,xorid,yorid
	 real*4 spval,dx,dy,xori,yori
	 character*80 record

         spval=spvald
	 dx=dxd
	 dy=dyd
	 xori=xorid
	 yori=yorid

	 xori=xori+dx
         yori=yori+dy

         nt=1
	 ndim=1 
	 icod=1

	 write(iu) record
	 write(iu) record
	 write(iu) record
	 write(iu) record

	 write(iu) nx,ny,nz,nt,ndim,icod
	 write(iu) xori,yori,dx,dy,spval
	 write(iu) (float(k),k=1,nz)

	 do l=1,nt
	   write(iu) float(l)
	   do k=1,nz
	      write(iu) ((sngl(c8(i,j,k)),i=1,nx),j=1,ny)
           enddo
	 enddo
 
	 return
	 end subroutine

! Procedure 104
! ------------

!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  TOPOLO (MODULE)
!C     -  RDTOPO (read the topology of the finite element mesh: unit 11)
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             TOPOL0 MODULE                            C
!C       Description of the topology of the finite element grid         C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine topolo(ipr)
      include'divapre.h'
      include'divainc.h'
!C
!C  INPUT OF GENERAL DATA
!C
      read(10,*) nnt1
      if(ipr.gt.0) write(6,*) ' Total number of vertex nodes :',nnt1
      read(10,*) nnint
      if(ipr.gt.0) write(6,*) ' Total number of interfaces   :',nnint
      nnt=nnt1+nnint
      if(ipr.gt.0) write(6,*) ' Total number of nodes        :',nnt
      read(10,*) nelt
      if(ipr.gt.0) write(6,*) ' Total number of elements     :',nelt
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KLINK(I)    : OPTIMAL POSITION OF NODE (I) IN THE SEQUENCE OF DOF
!C  ==> KSORT(IPOSI): NODE ID IN POSITION (I) IN THE SEQUENCE OF NODES
!C  ==> TCOOG(I,*)  : TABLE OF ABSLUTE COORDINATES OF NODE (I)
!C
      call allody(nnt,0,'klink',lklink,ipr)
      call allody(nnt,0,'ksort',lksort,ipr)
      call allody(2*nnt1,1,'tcoog',ltcoog,ipr)
!C
!C ALLOCATION OF STORAGE TABLES:
!C  ==> KSKYH (I)   :CUMULATED HEIGHT OF STIFFNESS MATRIX COLUMN
!C  ==> KCONN (I,*) :CONNECTIVITY TABLE BETWEEN ELEMENTS AND NODES
!C  ==> KLOCE (I)   :LOCALIZATION OF ONE ELEMENT IN THE STRUCTURE
!C
!C FINITE ELEMENT ITYP=2 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.2) then
         nnel=6
         nddle=12
         nddlt=3*nnt1+nnint
      endif
!C
!C FINITE ELEMENT ITYP=3 (FVD) (see: SANDER, Ph.D. Dissertation, 1969)
!C
      if(ityp.eq.3) then
         nnel=8
         nddle=16
         nddlt=3*nnt1+nnint
!C SPACE ALLOCATION FOR CENTER OF ELEMENT (CALCULATED ONCE)
         call allody(2*nelt,1,'tcele',ltcele,ipr)
      endif
!C
!C WHATEVER THE TYPE OF ELEMENT ...
!C
      if(ipr.gt.0) write(6,*) ' Total number of deg. of frd. :',nddlt
      call allody(nddlt+1,0,'kskyh',lkskyh,ipr)
      call allody(nnel*nelt,0,'kconn',lkconn,ipr)
      if (ltcoog.le.0) then
      write(6,*) 'ltcoog',ltcoog
      goto 321
      endif
      if (ltcele.le.0) then
      if(ityp.eq.3) then
      write(6,*) 'ltcele??',ltcele
      endif
      bidon=0
      
      call rdtopo(s(ltcoog),l(lkconn),l(lklink),l(lksort),bidon,ipr)
      
                       else
      
      call rdtopo(s(ltcoog),l(lkconn),l(lklink),l(lksort),s(ltcele),ipr)
      endif
 321  continue
      if (icoordchange.ne.0) call topollxy(s(ltcoog),ipr)
      call allody(nddle,0,'kloce',lkloce,ipr)
      call calsky(l(lkskyh),l(lkconn),l(lklink),l(lkloce),ipr)

      return
      end subroutine
      
! Procedure 105
! ------------
      

      subroutine rdtopo(tcoog,kconn,klink,ksort,tcele,ipr)
!C
!C  I/O OF TOPOLOGIC DATA SET
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),klink(nnt),ksort(nnt),tcele(nelt,2)
!C
!C  INPUT OF ID and COORDINATES of VERTEX NODES
!C
      do 10 i=1,nnt1
         read(11,*) ksort(i),tcoog(i,1),tcoog(i,2)
 10   continue
!C
!C  INPUT OF ID of INTERFACE NODES
!C
      do 20 i=1+nnt1,nnt
         read(11,*) ksort(i)
 20   continue
!C
!C  INPUT OF CONNECTIVITY TABLE
!C
      do 30 i=1,nelt
         read(11,*) (kconn(i,j),j=1,nnel)
 30   continue
!C
!C  RE-DEFINE KCONN IF ELEMENTS ARE NOT LEFT-AREA ORIENTED
!C              NECESSARY WHEN ITYP = 2 OR 3
!C
      if(ityp.eq.2) then
         zero=0.
         do 35 iel=1,nelt
            x1=tcoog(kconn(iel,1),1)
            x2=tcoog(kconn(iel,3),1)
            x3=tcoog(kconn(iel,5),1)
            y1=tcoog(kconn(iel,1),2)
            y2=tcoog(kconn(iel,3),2)
            y3=tcoog(kconn(iel,5),2)
            dotvz=(x2-x1)*(y3-y2)-(x3-x2)*(y2-y1)
            if(dotvz.lt.zero) then
               intf1=kconn(iel,6)
               intf3=kconn(iel,2)
               iver2=kconn(iel,5)
               iver3=kconn(iel,3)
               kconn(iel,2)=intf1
               kconn(iel,6)=intf3
               kconn(iel,3)=iver2
               kconn(iel,5)=iver3
               if (ipr.ge.3) then
                  write(6,*) ' !!! ELEMENT ',IEL,' WAS LEFT-ORIENTED !'
               endif
            endif
 35      continue
      endif
      if(ityp.eq.3) then
         zero=0.
         do 36 iel=1,nelt
            x1=tcoog(kconn(iel,1),1)
            x2=tcoog(kconn(iel,3),1)
            x3=tcoog(kconn(iel,5),1)
            x4=tcoog(kconn(iel,7),1)
            y1=tcoog(kconn(iel,1),2)
            y2=tcoog(kconn(iel,3),2)
            y3=tcoog(kconn(iel,5),2)
            y4=tcoog(kconn(iel,7),2)
            call intsec(x1,x2,x3,x4,y1,y2,y3,y4,x0,y0)
            tcele(iel,1)=x0
            tcele(iel,2)=y0
            dotvz=(x2-x1)*(y3-y2)-(x3-x2)*(y2-y1)
            if(dotvz.lt.zero) then
               intf1=kconn(iel,8)
               intf2=kconn(iel,6)
               intf3=kconn(iel,4)
               intf4=kconn(iel,2)
               iver2=kconn(iel,7)
               iver4=kconn(iel,3)
               kconn(iel,2)=intf1
               kconn(iel,4)=intf2
               kconn(iel,6)=intf3
               kconn(iel,8)=intf4
               kconn(iel,3)=iver2
               kconn(iel,7)=iver4
               if (ipr.ge.3) then
                  write(6,*) ' !!! ELEMENT ',IEL,' WAS LEFT-ORIENTED !'
               endif
            endif
 36      continue
      endif
!C
!C  CONSTRUCTION OF KLINK VECTOR; FOR TYPE 2 OR 3: FDV ELEMENT
!C
      if(ityp.eq.2.or.ityp.eq.3) then
         ilink=1
         do 40 iposi=1,nnt
            inod=ksort(iposi)
            klink(inod)=ilink
            ilink=ilink+1
            if(inod.le.nnt1) ilink=ilink+2
 40      continue
      endif
!C
!C OUTPUT OF TOPOLOGICAL DATA
!C
      if(ipr.ge.3) then
         write(6,*)' List of vertex nodes id, X and Y positions'
         write(6,*)' ------------------------------------------'
         do 100 i=1,nnt1
           write(6,*) i,tcoog(i,1),tcoog(i,2)
 100     continue
      endif
      if(ipr.ge.3) then
         if(ityp.eq.2) then
            write(6,*)' List of element id and connectivities '
            write(6,*)' ------------------------------------- '
            do 110 i=1,nelt
              write(6,910) i,(kconn(i,j),j=1,nnel)
 910          format(' Element :',i5,'  Nodes :',10(i5))
 110        continue
         endif
         if(ityp.eq.3) then
            write(6,*)' List of element id , center and connectivities'
            write(6,*)' ----------------------------------------------'
            do 115 i=1,nelt
              write(6,915) i,(tcele(i,j),j=1,2),(kconn(i,j),j=1,nnel)
 915          format(' Element:',i4,' Center:',2(f7.1),' Nodes:',10(i4))
 115        continue
         endif
      endif
      if(ipr.ge.4) then
         write(6,*)' I   ,    KSORT(I)    ,     KLINK(I)      '
         write(6,*)' ---------------------------------------- '
         do 120 i=1,nnt
           write(6,*) i,ksort(i),klink(i)
 120     continue
      endif
      return
      end subroutine

! Procedure 106
! ------------

!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
!C   SUBROUTINE LIST:
!C     -  STORES (MODULE)
!C     -  EXTRT2 (extract the solution from an ITYP=2 element)
!C     -  EXTRT3 (extract the solution from an ITYP=3 element)
!C     -  GENF80 (create fort.80 file, with localization of solution
!C               points in the finite element mesh
!C
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                      C
!C                             STORES MODULE                            C
!C                         Storage of the solution                      C
!C                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine stores(ipr)
      include'divapre.h'
      real*8 c8,gcvaln,vargcv
      include'divainc.h'
      real*8 d0d,d1d,d2d,d3d,d4d,atr,btr,ctr
      common/GCV/GCVALN,vargcv,d0d,d1d,d2d,d3d,d4d,atr,btr,ctr,nntr
      GCVALN=0
      vargcv=0
      d0d=0
      d1d=0
      d2d=0
      d3d=0
      d4d=0
      atr=0
      btr=0
      ctr=0
      ifirst=1
!C
!C  PROBLEM P2: OPEN THE FILE CONTAINING THE CONDENSATION VECTOR
!C
!c      if(ityp.eq.2.or.ityp.eq.3) then
!c         open(32,file='kele2.cnd',recl=nddle*3*iprc,form='unformatted',
!c     &        access='direct')
!C         write(6,*) 'JMB: depreciated file output'
!c         if (IJMB.EQ.0) STOP 'KELE OPTIMISATION NOT WORKING'
!c      endif
!C
!C  input of general data
!C
      read(10,*) ispec
!C      if(ipr.gt.0) write(6,*) ' Specification of output =',ispec
      ispec0=ispec
      ispec=0
      
!CJMBB Produce analysis at data points into fort.71
         rewind(13)
         read(13,*,end=1234) xori,yori
         read(13,*)dx,dy
         read(13,*)nx,ny
         read(13,*)valex
         rewind(13)
 1234 continue
      if(ispec0.eq.0.or.ispec0.eq.3.or.ispec0.eq.4) then
         ireclu=0
 6       read(79,*,end=8) x,y
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c               write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
               write(82,*) x_ll,y_ll,valex
               goto 6
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(1),ipr,s(lrkele))
!C            if (icoordchange.ne.0) call xyll(x,y)
            write(82,*) x_ll,y_ll,val
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            write(82,*) x_ll,y_ll,val
            goto 6
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(1),ipr,s(lrkele))
!C            if (icoordchange.ne.0) call xyll(x,y)
            write(82,*) x_ll,y_ll,val
         endif
         goto 6
 8       continue

 123     continue
         ireclu=0
         ijmbval=0
         rewind(20)
 66      read(20,*,end=86) x,y,tttt,wwww
         x_ll=x
         y_ll=y
         if (icoordchange.ne.0) call llxy(x,y)
         ireclu=ireclu+1
         if(ityp.eq.2) then
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,s(ltcoog),l(lkconn),iel,isub,l(lkntc),ipr)
!c               if (iel.eq.-1) then
!c                write(6,*) 'sauve qui store',x_ll,y_ll
!c                call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
!c               endif
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,s(ltcoog),l(lkconn),iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
!C               if (icoordchange.ne.0) call xyll(x,y)
!c              write(6,*) ' Donnee ',ireclu,x_ll,y_ll,' non localisee'
!C               write(82,*) x_ll,y_ll,-9999.0
                val=valex
                write(71,*) x_ll,y_ll,val
               goto 66
            endif
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(1),ipr,s(lrkele))
!C            if (icoordchange.ne.0) call xyll(x,y)
            write(71,*) x_ll,y_ll,val
         endif
         if(ityp.eq.3) then
            if (opti.eq.1) then 		! (SvL)
            call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
            call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif

            if(iel.le.0.or.isub.le.0) then
            val=valex
            write(71,*) x_ll,y_ll,val
            goto 66
            endif
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(1),ipr,s(lrkele))
!C            if (icoordchange.ne.0) call xyll(x,y)
            write(71,*) x_ll,y_ll,val
         endif
         ijmbval=ijmbval+1
         jmboff=ndata*2+ireclu-1
         valb=val
         IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            rewind(22)
            CLOSE (22)
            
            IFIRST = 0
         ENDIF
         VALb = VAL - XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            rewind(22)
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            rewind(22)
            IFIRST = 0
         ENDIF
         VALb = VAL - A0 - A1*X - A2*Y
      ENDIF

!c         write(6,*) 'datasto',s(ltdata+jmboff),jmboff,ireclu,val
         GCVALN=GCVALN+(s(ltdata+jmboff)-valb)*wwww/hmmu*(s(ltdata+jmboff)-valb)
         vargcv=vargcv+(s(ltdata+jmboff))*(s(ltdata+jmboff))
         d0d=d0d+wwww/hmmu*s(ltdata+jmboff)*s(ltdata+jmboff)
         d1d=d1d+wwww/hmmu*s(ltdata+jmboff)*valb
         d2d=d2d+wwww/hmmu*valb*valb
         goto 66
  86     continue
         GCVALN=sqrt(GCVALN/ijmbval)
         vargcv=(vargcv/ijmbval)
         d0d=d0d/ijmbval
         d1d=d1d/ijmbval
         d2d=d2d/ijmbval
         nntr=ijmbval
         write(33,*) vargcv,ijmbval
!CJMBE
 
 
      endif
      ispec=ispec0
      if(ispec.ge.1) then
         read(13,*)xori,yori
         read(13,*)dx,dy
         read(13,*)nx,ny
         read(13,*)valex
!c                  write(6,*) 'xorib',xori, yori,dx,dy
         
         iix=1
         iiy=0
         if (icoordchange.ne.0) then
          xorio=xori
          call llxy(xori,yori)
          dx=dx*dxkm 
          dy=dy*dykm 
         endif
!c         write(6,*) 'xori',xori, yori,dx,dy
!C
!C  ALLOCATION OF SPACE FOR THE GRIDDED SOLUTION
!C
         call allody(nx*ny,1,'tgrds',ltgrds,ipr)
         do 5 i=1,nx*ny
            s(ltgrds+i-1)=valex
 5       continue
      endif

!C
!C  construction of file 80
!C
      if(ispec.eq.1.or.ispec.eq.3) then
         if(ltcele.eq.0) then
         bidon=0
         call genf80(s(ltcoog),l(lkconn),l(lkntc),bidon,ipr)
         else
         call genf80(s(ltcoog),l(lkconn),l(lkntc),s(ltcele),ipr)
      endif
         close(80)
         open(unit=80,file='fort.80')
      endif
      if(ispec.gt.0) then
      index=0
!C JMB (change for the problem in extre2 for regular grid...
 10    continue
       val=valex
       if(ityp.eq.2) then
    
         read(80,*,end=100)x,y,iel,isub
!cmr         write(6,*)x,y,iel,isub
!C         if (icoordchange.ne.0) call llxy(x,y)
         if(iel.gt.0) then
            call extrt2(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),val,s(ltgrds),ipr,s(lrkele))
         endif
      endif
      if(ityp.eq.3) then
         read(80,*,end=100)x,y,iel,isub
!C         if (icoordchange.ne.0) call llxy(x,y)
         if(iel.gt.0) then
            call extrt3(x,y,iel,isub,s(ltcoog),l(lkconn),l(lkloce),l(lklocs),s(ltrhsg),s(ltcele),val,s(ltgrds),ipr,s(lrkele))
         endif
      endif
      index=index+1
!C JMB to replace the regular grid output
      ix=nint((x-xori)/dx)
      iy=nint((y-yori)/dy)

      iiy=iiy+1
      if(iiy.gt.ny) then
      iiy=1
      iix=iix+1
      endif
      
!c      if (isspheric.eq.1) then
!c      xori=xorio
!c      yyy=y
!c      call llxy(xori,yyy)
!c      write(6,*) 'Spheric',xori,dx
!c      xxx=max(cos(y*RPI/180),1E-10)
!c      ix=nint((x-xori)/dx/xxx)
!c      endif
!c      if(iix.ne.ix.or.iiy.ne.iy) then
!c      write(6,*) '????',ix,iix,iy,iiy
!c      endif
       ix=iix
       iy=iiy
       jmboff=(iy-1)*nx+ix-1
!c      write(6,*) 'gridded',ix,iy,jmboff,val
      s(ltgrds+jmboff)=val
!C JMBend??
      goto 10
 100  nptsol=index
      write(6,*)'Total nb. of pts where gridded solution is asked =',nptsol
      endif
      if(ispec.ge.1) then
         call impmat(s(ltgrds),nx,ny,nx,83)
         iu=84
        nbm=-1
!c        write(6,*) 'storing', iprc
         if(iprc.eq.4) then
           call uwritc(iu,c8,s(ltgrds),valex,iprc,nx,ny,1,nbm)
           write(6,*) 'Storing field estimate in real*4'
                       else
!c           write(6,*) 'Storing field estimate in real*8'
!c,nx,ny,nbm
           call uwrit2(iu,c8,s(ltgrds),valex,iprc,nx,ny,1,nbm)  
!c           write(6,*)' % Writing gridded field in single precision % '
!c           write(6,*) 'Storing field estimate in real*8'
         endif
         write(6,*) 'Finished storing'
      endif
      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
      return
      end subroutine
      
! Procedure 106
! ------------
      



      subroutine extrt2(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,val,tgrds,ipr,trkele)
!C
!C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=2
!C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
!C            IS COMPULSORY                               !!!!!!!!!!!
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(3,10),  &
               sol(nddlt),tr(3,12),ddl(15),x(0:3),y(0:3),ddlsub(10),      &
               tjac(2,2),tjaci(2,2),wk(10,10),tgrds(nx,ny),ep(10)          &
               ,TRKELE(3,12,*)
      REAL*4 XMEAN, A0, A1, A2
      INTEGER IFIRST
      SAVE IFIRST, XMEAN, A0, A1, A2

      DATA IFIRST / 1 /
!cmr      write(6,*) 'element bef: ',iel

      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(0)=(x(1)+x(2)+x(3))/3.
      y(0)=(y(1)+y(2)+y(3))/3.
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
!cmr      write(6,*) 'element : ',iel
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c         endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
      do 10 i=1,12
        ddl(i)=sol(kloce(i))
 10   continue
!C
!C  CORRECTION FOR NORMAL REFERENCE
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(10)=-ddl(10)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(11)=-ddl(11)
      endif
!C mr BIG BUG      if(kconn(iel,1).lt.kconn(iel,3)) then
      if(kconn(iel,1).lt.kconn(iel,5)) then
         ddl(12)=-ddl(12)
      endif
      do 20 i=1,3
         ddl(12+i)=zero
         do 25 k=1,12
            ddl(12+i)=ddl(12+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
!C
!C calculate the (XI,ETA) coordinates from (X,Y)
!C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
!C
!C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
!C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
!C
!C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue

!C --- Code Added for Version 2.2  (RS 22 March 94) ---

      IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            IFIRST = 0
         ENDIF
!c          val=val
         VAL = VAL + A0 + A1*XP + A2*YP
!C test: only reference valye
!c         VAL=A0 + A1*XP + A2*YP
      ENDIF

!C --- End of Added Code ---
!C JMB???????? what does this added code mean?????? check for ispec history
!C when putting out also in discrete locations. There is no need to fill in
!C the regular grid when asking for local values only (effet de bord assure...)
!c      if(ispec.ge.1) then
!c        ix=nint((xp-xori)/dx)
!c        iy=nint((yp-yori)/dy)
!c        tgrds(ix,iy)=val
!c      endif
      return
      end subroutine

! Procedure 107
! ------------


      subroutine extrt3(xp,yp,iel,isub,tcoog,kconn,kloce,klocs,sol,tcele,val,tgrds,ipr,trkele)
!C
!C  EXTRACTION OF THE SOLUTION FROM AN ELEMENT WITH ITYP=3
!C  !!!!!!  A CHANGE OF COORDINATES (FROM GLOBAL TO REFERENCE SYSTEM)
!C            IS COMPULSORY                               !!!!!!!!!!!
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),kloce(nddle),klocs(4,10),  &
               sol(nddlt),tr(3,16),ddl(19),x(0:4),y(0:4),ddlsub(10),      &
               tjac(2,2),tjaci(2,2),wk(10,10),tgrds(nx,ny),                &
               tcele(nelt,2),ep(10)                                         &
               ,TRKELE(3,12,*)

      REAL*4 XMEAN, A0, A1, A2
      INTEGER IFIRST
      SAVE IFIRST, XMEAN, A0, A1, A2

      DATA IFIRST / 1 /

      zero=0.D0
      un=1.D0
      deux=2.D0
      trois=3.D0
      quatre=4.D0
      x(1)=tcoog(kconn(iel,1),1)
      y(1)=tcoog(kconn(iel,1),2)
      x(2)=tcoog(kconn(iel,3),1)
      y(2)=tcoog(kconn(iel,3),2)
      x(3)=tcoog(kconn(iel,5),1)
      y(3)=tcoog(kconn(iel,5),2)
      x(4)=tcoog(kconn(iel,7),1)
      y(4)=tcoog(kconn(iel,7),2)
      x(0)=tcele(iel,1)
      y(0)=tcele(iel,2)
      call calloc(iel,kloce,nddle,kconn,l(lklink),ipr)
!c      read(32,rec=iel) tr
!C JMB
!c      read(32,rec=iel) tr
!c            if(iel.gt.JMBELE) then
!c        write(*,*) 'INCREASE JMBELE to at least',IEL
!c        stop
!c        endif
        do i=1,3
         do j=1,12
          tr(i,j)=trkele(i,j,iel)
          enddo
        enddo
!C JMB
      do 10 i=1,16
        ddl(i)=sol(kloce(i))
 10   continue
!C
!C  CORRECTION FOR NORMAL REFERENCE
!C
      if(kconn(iel,3).lt.kconn(iel,1)) then
         ddl(13)=-ddl(13)
      endif
      if(kconn(iel,5).lt.kconn(iel,3)) then
         ddl(14)=-ddl(14)
      endif
      if(kconn(iel,7).lt.kconn(iel,5)) then
         ddl(15)=-ddl(15)
      endif
      if(kconn(iel,1).lt.kconn(iel,7)) then
         ddl(16)=-ddl(16)
      endif
      do 20 i=1,3
         ddl(16+i)=zero
         do 25 k=1,16
            ddl(16+i)=ddl(16+i)+tr(i,k)*ddl(k)
 25      continue
 20   continue
!C
!C calculate the (XI,ETA) coordinates from (X,Y)
!C
      x0=x(0)
      y0=y(0)
      if(isub.eq.1) then
         x1=x(1)
         y1=y(1)
         x2=x(2)
         y2=y(2)
      endif
      if(isub.eq.2) then
         x1=x(2)
         y1=y(2)
         x2=x(3)
         y2=y(3)
      endif
      if(isub.eq.3) then
         x1=x(3)
         y1=y(3)
         x2=x(4)
         y2=y(4)
      endif
      if(isub.eq.4) then
         x1=x(4)
         y1=y(4)
         x2=x(1)
         y2=y(1)
      endif
      do 30 i=1,10
         ddlsub(i)=ddl(klocs(isub,i))
 30   continue
!C
!C  EVALUATION OF SHAPE FUNCTIONS AT SOLUTION POINT
!C
      tjac(1,1)=x1-x0
      tjac(2,1)=x2-x0
      tjac(1,2)=y1-y0
      tjac(2,2)=y2-y0
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      tjaci(1,1)=(y2-y0)/detj
      tjaci(2,2)=(x1-x0)/detj
      tjaci(1,2)=-(y1-y0)/detj
      tjaci(2,1)=-(x2-x0)/detj
      xi=tjaci(1,1)*(xp-x0)+tjaci(2,1)*(yp-y0)
      eta=tjaci(1,2)*(xp-x0)+tjaci(2,2)*(yp-y0)
      call ep2(xi,eta,ep)
!C
!C  TRANSFORMATION FROM GLOBAL TO REFERENCE COORDINATE SYSTEM
!C
      dist12=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      p1=(y2-y1)*(x1+x2-deux*x0)-(x2-x1)*(y1+y2-deux*y0)
      p1=p1/(sqrt(deux)*dist12)
      p2=(y2-y1)*(y1+y2-deux*y0)+(x2-x1)*(x1+x2-deux*x0)
      p2=p2/(sqrt(deux)*dist12)
      do 35 i=1,10
         do 36 j=1,10
            wk(i,j)=zero
 36      continue
 35   continue
      do 40 k=1,3
         is=(k-1)*3+1
         wk(is,is)=un
         do 41 i=1,2
            do 42 j=1,2
               wk(is+i,is+j)=tjac(i,j)
 42         continue
 41      continue
 40   continue
      wk(10,10)=p1
      wk(10,4)=-trois*p2/(deux*dist12)
      wk(10,7)=-wk(10,4)
      wk(10,5)=p2*(x1-x2)/(quatre*dist12)
      wk(10,8)=wk(10,5)
      wk(10,6)=p2*(y1-y2)/(quatre*dist12)
      wk(10,9)=wk(10,6)
      val=zero
      do 100 i=1,10
         vc=zero
         do 110 k=1,10
            vc=vc+wk(i,k)*ddlsub(k)
 110     continue
         val=val+vc*ep(i)
 100  continue

!C --- Code Added for Version 2.2  (RS 22 March 94) ---

      IF (IREG.EQ.1) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) XMEAN
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + XMEAN
      ENDIF

      IF (IREG.EQ.2) THEN
         IF (IFIRST.EQ.1) THEN
            OPEN (UNIT=22,FILE='fort.22')
            READ (22,*) A0,A1,A2
            CLOSE (22)
            IFIRST = 0
         ENDIF
         VAL = VAL + A0 + A1*XP + A2*YP
      ENDIF

!C --- End of Added Code ---


!C Also strange when extraction in localized points... need to add some test here
!c      if(ispec.ge.1) then
!c        ix=nint((xp-xori)/dx)
!c        iy=nint((yp-yori)/dy)
!c        tgrds(ix,iy)=val
!c      endif
      return
      end subroutine

! Procedure 107
! ------------



      subroutine genf80(tcoog,kconn,kntc,tcele,ipr)
!C
!C  THIS ROUTINE GENERATES A LIST OF POINTS (X,Y,IEL,ISUB) ON A REGULAR
!C  GRID, WHERE THE SOLUTION IS REQUIRED
!C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),kconn(nelt,nnel),tcele(nelt,2)
!C
!C  FOR ITYP = 2 ...
!C
      if(ityp.eq.2) then
      do 10 i=1,nx
         do 15 j=1,ny
      
            x=xori+i*dx
            y=yori+j*dy
            if (isspheric.eq.1) then
            xori=xorio
            yyy=y
            call llxy(xori,yyy)
            x=xori+i*dx*cos(y*RPI/180.)
            endif
!Cmr            if (icoordchange.ne.0) call llxy(x,y)
            if (opti.eq.1) then 		! (SvL)
               call locpt2opti(x,y,tcoog,kconn,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
               call locpt2(x,y,tcoog,kconn,iel,isub,ipr)
            endif
            write(80,*)x,y,iel,isub
!cmr            write(6,*)'create 80 ',x,y,iel,isub
 15      continue
 10   continue
      endif
!C
!C  FOR ITYP = 3 ...
!C
      if(ityp.eq.3) then
      do 110 i=1,nx
         do 115 j=1,ny
            x=xori+i*dx
            y=yori+j*dy
            if (isspheric.eq.1) then
            xori=xorio
            yyy=y
            call llxy(xori,yyy)
            x=xori+i*dx*cos(y*RPI/180.)
            endif
!Cmr            if (icoordchange.ne.0) call llxy(x,y)
            if (opti.eq.1) then 		! (SvL)
               call locpt3opti(x,y,tcoog,kconn,tcele,iel,isub,kntc,ipr)
            endif
            if (opti.eq.0) then 
               call locpt3(x,y,tcoog,kconn,tcele,iel,isub,ipr)
            endif
            write(80,*)x,y,iel,isub
 115     continue
 110  continue
      endif
      return
      end subroutine

      
      

END MODULE moduleCalc

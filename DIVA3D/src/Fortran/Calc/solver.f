C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   SUBROUTINE LIST:
C     -  SOLVER (MODULE)
C     -  CALKEL : calculating and assembling elementary matrices
C     -  CDIR   : calculating cos-directions
C     -  CKELE2 : integrating elementary matrices when ITYP=2
C     -  CKELE3 : integrating elementary matrices when ITYP=3
C     -  CKSEL2 : integrating sub-elementary matrices when ITYP=2
C     -  FIXBCD : fixe the boundary conditions (Dirichlet type)
C     -  LOCSE2 : localisation of sub-elements in the ITYP=2 element
C     -  LOCSE3 : localisation of sub-elements in the ITYP=3 element
C     -  IMPSOL : print the solution at principal connectors
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             SOLVER MODULE                            C
C       Build and solve the global linear finite element system        C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine solver(ipr)
      include'divapre.h'
      include'divainc.h'
C
C  INPUT OF GENERAL DATA
C
      write(6,*) 'into solver',ipr
      read(10,*) istf 
      if (istf.ne.0) then
      write(6,*) 'Stiffness parameter ISTF',istf
      endif
C
C ALLOCATION OF STORAGE TABLES:
C  ==> TUPPE   : UPPER VECTOR CONTAINING THE GLOBAL LINEAR SYSTEM
C  ==> TLOWE   : LOWER VECTOR OF THE GLOBAL LINEAR SYSTEM (IF ISYM=0)
C  ==> TDIAG   : DIAGONAL VECTOR OF THE GLOBAL LINEAR SYSTEM
C  ==> TRHSG   : RIGHT HAND SIDE OF THE GLOBAL LINEAR SYSTEM
C  ==> TKELE   : ELEMENTARY MATRIX (NDDLE*NDDLE)
C  ==> TRHSE   : RIGHT HAND SIDE OF THE ELEMENTARY SYSTEM
C
      call allody(nelt,1,'tstif',ltstif,ipr)
C      call allody(nterm,1,'tuppe',ltuppe,ipr)
C      call allody(nddlt,1,'tdiag',ltdiag,ipr)
C JMB2013 moved allocation so that upper and diagonal are one after the other
C and can be exploited in new solver
C      call allody(nterm,1,'tuppe',ltuppe,ipr)
C      call allody(nddlt,1,'tdiag',ltdiag,ipr)
       call allody(nterm+nddlt,1,'tuppe',ltuppe,ipr)
       ltdiag=ltuppe+nterm
C JMB2013
      if(isym.eq.0) then
         call allody(1,1,'tlowe',ltlowe,ipr)
                    else
         call allody(nterm,1,'tlowe',ltlowe,ipr)
      endif
      
      call allody(nddlt,1,'trhsg',ltrhsg,ipr)
      call allody(nddle*nddle,1,'tkele',ltkele,ipr)
      call allody(nddle,1,'trhse',ltrhse,ipr)
C
C  CALCULATION OF ELEMENTARY MATRICES AND INTEGRATION
C              IN THE GLOBAL SYSTEM
C
      call calkel(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),s(ltcoog),
     &            s(ltkele),s(ltrhse),l(lkconn),l(lkskyh),s(ltstif),
     &            ipr)
C
C  FIX BOUNDARY CONDITIONS
C
      if(ltcndi.eq.0) then
      bidon=0
      ibidon=0
      call fixbcd(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),l(lklink),
     &            l(lkskyh),bidon,ibidon)

                       else

      call fixbcd(s(ltuppe),s(ltlowe),s(ltdiag),s(ltrhsg),l(lklink),
     &            l(lkskyh),s(ltcndi),l(lkcndi))
      endif
      
C
C  SOLVE THE LINEAR SYSTEM (SKYLINE METHOD)  (Dhatt & Touzot, 1985)
C
      ifac=1
      isol=1
      mp=6
      call sol(s(ltuppe),s(ltdiag),s(ltlowe),s(ltrhsg),l(lkskyh),nddlt,
     &        mp,ifac,isol,isym,energ)
      if(ipr.ge.1) then
         call impsol(s(ltcoog),l(lklink),s(ltrhsg))
      endif
      return
      end




      subroutine calkel(tuppe,tlowe,tdiag,trhsg,tcoog,tkele,trhse,
     &                  kconn,kskyh,tstif,ipr)
C
C  LOOP FOR EVERY ELEMENT: CALCULATION OF ELEMENTARY MATRICES AND
C  ASSEMBLING IN THE GLOBAL SYSTEM
C
      include'divapre.h'
      include'divainc.h'
      dimension tuppe(nterm),tlowe(nterm),tdiag(nddlt),trhsg(nddlt),
     &          tcoog(nnt1,2),tkele(nddle,nddle),trhse(nddle),
     &          kconn(nelt,nnel),kskyh(nddlt+1),tstif(nelt),
     &          xg(16),yg(16),wg(16)
      one=1.0D0
      three=3.0D0
      five=5.0D0
C
C  READ ELEMENTARY STIFNESSES WHEN ISTF = 1 (PSEUDO-MODIF. OF TOPOLOGY) 
C  DEFAULT VALUE = 1
C
      do 2 iel=1,nelt
         tstif(iel)=one        
 2    continue
      if(istf.eq.1) then
         do 5 iel=1,nelt
            read(60,*)tstif(iel)
 5       continue
      endif
C
C  EVALUATION OF SHAPE FUNCTIONS AT GAUSS INTEGRATION POINTS
C  AND AT INTERIOR INTERFACES BETWEEN SUB-ELEMENTS
C   Maximum : 16 Gauss Integration Points
C======================================================================
C  SHAPE FUNCTIONS WHEN ITYP = 2 OR 3: NG = 4 (HAMMER RULE)
C  ALLOCATION OF STORAGE ARRAYS:
C   ==> TSHAG(I,J,K) = VALUE OF SHAPE FUNCTION I, IRULE=J,
C       GAUSS P OR INTERIOR INTERFACE = K
C         (IRULE=:1=>VALUE; 2=>DX; 3=>DY; 4=>DDX; 5=>DDY; 6=>DDXY;
C          7=>  DX AT INTERIOR INTERFACES;
C          8=>  DY AT INTERIOR INTERFACES)
C   ==> KLOCS(I,J) = ARRAY FOR LOCALISATION OF SUB-ELEMENTS IN ELEMENTS
C
C  WHEN ITYP.EQ.3 ...
C
      if(ityp.eq.2) then
         ng=4
         call allody(10*8*ng,1,'tshag',ltshag,ipr)
         call allody(30,0,'klocs',lklocs,ipr)
C
C  NUMERICAL INTEGRATION : HAMMER METHOD ON A TRIANGLE (Dhatt & Touzot)
C    FOR 3rd ORDER POLYNOMIAL BASIS: 4 Gauss Points
C
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
C
C  CONSTRUCTION OF LOCALISATION ARRAY FOR SUB-ELEMENTS
C
         call locse2(l(lklocs))
      endif
C
C  WHEN ITYP.EQ.3 ...
C
      if(ityp.eq.3) then
         ng=4
         call allody(10*8*ng,1,'tshag',ltshag,ipr)
         call allody(40,0,'klocs',lklocs,ipr)
C
C  NUMERICAL INTEGRATION : HAMMER METHOD ON A TRIANGLE (Dhatt & Touzot)
C    FOR 3rd ORDER POLYNOMIAL BASIS: 4 Gauss Points
C
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
C
C  CONSTRUCTION OF LOCALISATION ARRAY FOR SUB-ELEMENTS
C
         call locse3(l(lklocs))
      endif
      if(isym.eq.0) then
         nspace=nddle*(nddle+1)/2
                    else
         nspace=nddle*nddle
      endif
      call allody(nspace,1,'tvele',ltvele,ipr)
C
C  EVALUATION OF ELEMENTARY MATRIX WHEN ITYP = 2 OR 3
C
C  OPEN FILE FOR STORAGE OF CONDENSATION VECTORS (FOR DECONDENSATION)
C
c      if(ityp.eq.2.or.ityp.eq.3) then
c         open(32,file='kele2.cnd',recl=nddle*3*iprc,form='unformatted',
c     &        access='direct')
c      endif
C REPLACE by allody for TRKELE and pointer LRKELE
      call allody(3*12*NELT,1,'trkele',lrkele,ipr)
C now s(lrkele) for TRKELE
      do 10 iel=1,nelt
         if(ityp.eq.2) then
            if(ltprop.eq.0) then
            bidon=0
            call ckele2 (iel,tstif(iel),tcoog,kconn,tkele,trhse,
     &                   l(lklocs),
     &                   bidon,wg,ipr,s(lrkele))
            else
            call ckele2 (iel,tstif(iel),tcoog,kconn,tkele,trhse,
     &                   l(lklocs),
     &                   s(ltprop),wg,ipr,s(lrkele))
            endif
         endif
         if(ityp.eq.3) then
         if(ltprop.eq.0) then
            bidon=0
            call ckele3 (iel,tstif(iel),tcoog,kconn,tkele,trhse,
     &                   l(lklocs),
     &                   bidon,s(ltcele),wg,ipr,s(lrkele))
                         else
            call ckele3 (iel,tstif(iel),tcoog,kconn,tkele,trhse,
     &                   l(lklocs),
     &                   s(ltprop),s(ltcele),wg,ipr,s(lrkele))
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
C
C  LOCALIZATION IN THE STRUCTURE AND TRANSFORMATION OF THE MATRIX
C  ELEMENT IN VECTOR ELEMENT (REQUIRED BY ASSEL)
C  IF ISYM = 0, ONLY THE UPPER PART OF ELEMENTARY MATRIX IS REPRESENTED
C
         call calloc(iel,l(lkloce),nddle,l(lkconn),l(lklink),ipr)
         call append(tkele,nddle,s(ltvele),nspace,
     &               isym)
         ikg=1
         ifg=1
c         write(6,*) 'Into assel ',nddlt,kskyh(1)
         call assel(ikg,ifg,nddle,isym,l(lkloce),kskyh,s(ltvele),
     &              trhse,tuppe,tdiag,tlowe,trhsg)
 10   continue
      if(ityp.eq.2.or.ityp.eq.3) then
         close(32)
      endif
      return
      end



      subroutine cdir(dx,dy,rn,sn)
      include'divapre.h'
      dist=sqrt(dx*dx+dy*dy)
      rn=-dy/dist
      sn=dx/dist
      return
      end




      subroutine ckele2 (iel,stiff,tcoog,kconn,tkele,trhse,loces,tprop,
     &                   wg,ipr,TRKELE)
C
C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),tkele(12,12),trhse(12),kconn(nelt,nnel),
     &          x(0:3),y(0:3),wg(ng),tprop(nnt1,nnpr),u(0:3),v(0:3)
C
C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
C
      dimension tke(15,15),tge(15),tkse(10,10),tgse(10),loces(3,10),
     &          derx1(10),dery1(10),derx2(10),dery2(10),
     &          tcond(3,15),ro(3,3),tr(3,12),rw(3,12),romin(3,3)
     &          ,TRKELE(3,12,*),rll(0:3)
      toler=0.0001
C
C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS) - INIT.
C  CALCULATE NORMAL COMPONENTS OF INTERNAL INTERFACES
C
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
C
C  INTEGRATION OF THE 3 (10*10) SUB-ELEMENT MATRICES
C
      x0=x(0)
      y0=y(0)
      u0=u(0)
      v0=v(0)
      rll0=rll(0)
C
C  First sub-element
C
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
      call cksel2(1,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
      tcond(1,loces(1,i))=tcond(1,loces(1,i))+rn1*derx1(i)+sn1*dery1(i)
      tcond(2,loces(1,i))=tcond(2,loces(1,i))-rn2*derx2(i)-sn2*dery2(i)
      do 115 j=1,10
      tke(loces(1,i),loces(1,j))=tke(loces(1,i),loces(1,j))+tkse(i,j)
 115  continue
 110  continue
C
C  Second sub-element
C
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
      call cksel2(2,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
      tcond(2,loces(2,i))=tcond(2,loces(2,i))+rn2*derx1(i)+sn2*dery1(i)
      tcond(3,loces(2,i))=tcond(3,loces(2,i))-rn3*derx2(i)-sn3*dery2(i)
      do 125 j=1,10
      tke(loces(2,i),loces(2,j))=tke(loces(2,i),loces(2,j))+tkse(i,j)
 125  continue
 120  continue
C
C  Third sub-element
C
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
      call cksel2(3,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
      tcond(3,loces(3,i))=tcond(3,loces(3,i))+rn3*derx1(i)+sn3*dery1(i)
      tcond(1,loces(3,i))=tcond(1,loces(3,i))-rn1*derx2(i)-sn1*dery2(i)
      do 135 j=1,10
      tke(loces(3,i),loces(3,j))=tke(loces(3,i),loces(3,j))+tkse(i,j)
 135  continue
 130  continue
C
C             AND ELIMINATING THE D.O.F AT THE CENTER
C                       MATRIX CONDENSATION
C       (see method in: Brasseur, 1993, Ph. D. dissertation)
C
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
C
C  INVERSE OF RO
C
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
 
CJMB
C       write(32,rec=iel) tr
c       if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c       endif
       IJMB=1
       if(ipr.gt.5) write(6,*) 'saving trkele'
        do i=1,3
         do j=1,12
          trkele(i,j,iel)=tr(i,j)
c          ijmbw(iel)=1
          enddo
        enddo
C
C  REDUCTION OF ELEMENTARY MATRIX (15*15) ==> (12*12)
C
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
C
C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
C  WITH number(nod2) > number(nod1)
C
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
      end



      subroutine ckele3 (iel,stiff,tcoog,kconn,tkele,trhse,loces,tprop,
     &                   tcele,wg,ipr,TRKELE)
C
C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),tkele(16,16),trhse(16),kconn(nelt,nnel),
     &          x(0:4),y(0:4),wg(ng),tprop(nnt1,nnpr),u(0:4),v(0:4),
     &          tcele(nelt,2)
     &          ,TRKELE(3,12,*)
C
C  WORKING SPACE FOR SUB-ELEMENTARY MATRICES, ...
C
      dimension tke(19,19),tge(19),tkse(10,10),tgse(10),loces(4,10),
     &          derx1(10),dery1(10),derx2(10),dery2(10),
     &          tcond(3,19),ro(3,3),tr(3,16),rw(3,16),romin(3,3)
     &                         ,rll(0:4)
      toler=0.0001
C
C  DEFINE THE CENTER OF TRIANGLE (i.e., FOR SUB-ELEMENTS) - INIT.
C  CALCULATE NORMAL COMPONENTS OF INTERNAL INTERFACES
C
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
C
C  INTEGRATION OF THE 4 (10*10) SUB-ELEMENT MATRICES
C
      x0=x(0)
      y0=y(0)
      u0=u(0)
      v0=v(0)
      rll0=rll(0)
C
C  First sub-element
C
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
      call cksel2(1,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 110 i=1,10
      tge(loces(1,i))=tge(loces(1,i))+tgse(i)
      tcond(1,loces(1,i))=tcond(1,loces(1,i))+rn1*derx1(i)+sn1*dery1(i)
      tcond(2,loces(1,i))=tcond(2,loces(1,i))-rn2*derx2(i)-sn2*dery2(i)
      do 115 j=1,10
      tke(loces(1,i),loces(1,j))=tke(loces(1,i),loces(1,j))+tkse(i,j)
 115  continue
 110  continue
C
C  Second sub-element
C
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
      call cksel2(2,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 120 i=1,10
      tge(loces(2,i))=tge(loces(2,i))+tgse(i)
      tcond(2,loces(2,i))=tcond(2,loces(2,i))+rn2*derx1(i)+sn2*dery1(i)
      tcond(3,loces(2,i))=tcond(3,loces(2,i))-rn3*derx2(i)-sn3*dery2(i)
      do 125 j=1,10
      tke(loces(2,i),loces(2,j))=tke(loces(2,i),loces(2,j))+tkse(i,j)
 125  continue
 120  continue
C
C  Third sub-element
C
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
      call cksel2(3,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 130 i=1,10
      tge(loces(3,i))=tge(loces(3,i))+tgse(i)
      tcond(3,loces(3,i))=tcond(3,loces(3,i))+rn3*derx1(i)+sn3*dery1(i)
      do 135 j=1,10
      tke(loces(3,i),loces(3,j))=tke(loces(3,i),loces(3,j))+tkse(i,j)
 135  continue
 130  continue
C
C  Fourth sub-element
C
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
      call cksel2(4,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,l(lkindt),l(lkdata)
     &            ,l(lkelos),s(ltdata),s(ltshag),wg,ipr,derx1,dery1,
     &             derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &            l(lkindtQ),l(lkdataQ),l(lkelosQ),s(ltdataQ)  )
      do 131 i=1,10
      tge(loces(4,i))=tge(loces(4,i))+tgse(i)
      tcond(1,loces(4,i))=tcond(1,loces(4,i))-rn1*derx2(i)-sn1*dery2(i)
      do 136 j=1,10
      tke(loces(4,i),loces(4,j))=tke(loces(4,i),loces(4,j))+tkse(i,j)
 136  continue
 131  continue
C
C             AND ELIMINATING THE D.O.F AT THE CENTER
C                       MATRIX CONDENSATION
C       (see method in: Brasseur, 1993, Ph. D. dissertation)
C
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
C
C  INVERSE OF RO
C
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
C JMB
C      write(32,rec=iel) tr
c      if(iel.gt.JMBELE) then
c        write(*,*) 'INCREASE JMBELE to at least',IEL
c        stop
c        endif
        do i=1,3
         do j=1,12
          trkele(i,j,iel)=tr(i,j)
          enddo
        enddo
C JMB
C
C  REDUCTION OF ELEMENTARY MATRIX (19*19) ==> (16*16)
C
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
C
C  BEFORE ASSEMBLING, CHOICE OF A REFERENCE NORMAL: POINTING TO THE
C  RIGHT WHEN COVERING THE EXTERNAL INTERFACE FROM NOD1 TO NOD2,
C  WITH number(nod2) > number(nod1)
C
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
      end



C JMB2012 added some arguments *Q for source terms
      subroutine cksel2(isub,iel,tkse,tgse,x0,y0,x1,y1,x2,y2,kindt,
     &                  kdata,kelos,tdata,tshag,wg,ipr,derx1,dery1,
     &              derx2,dery2,u0,v0,u1,v1,u2,v2,stiff,rll0,rll1,rll2,
     &              kindtQ,kdataQ,kelosQ,tdataQ
     &                  )
C
C  INTEGRATE SUB-ELEMENT MATRIX WHEN ITYP = 2 (FDV ELEMENT)
C
      include'divapre.h'
      include'divainc.h'
      dimension tkse(10,10),tgse(10),kindt(nelt),kdata(ndata),
     &          kelos(ndata,2),tdata(ndata,4),tshag(10,8,ng),
     &          tjaci(2,2),t2j(3,3),tp(10),wg(ng),derx1(10),derx2(10),
     &          dery1(10),dery2(10),tshagn(10,8,4),tjac(2,2),
     &          tr(10,10),ttemp(10,10),gtemp(10),ep(10)
C JMB2012 source term
      dimension kindtQ(nelt),kdataQ(NSOURCES),
     &          kelosQ(NSOURCES,2),tdataQ(NSOURCES,4)
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
C
C  CALCULATION OF INVERSE JACOBIAN MATRIX , ...
C
      detj=(x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)
      if(detj.le.zero) then
         write(6,*) ' %%% ERROR - CKSEL2 : DET. JACOBIAN = ZERO %%%'
         write(6,*) 'Iel,isub,detj',iel,isub,detj
         write(6,*) 'x0,x1,x2,y0,y1,y2',x0,x1,x2,y0,y1,y2
         write(6,*) 'Will try to recover'
c         detj=(x2-x1)*(y0-y1)-(x0-x1)*(y2-y1)
c         write(6,*) 'Detj now',detj
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
C
C  For second derivatives: see Dhatt & Touzot (p57)
C
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
C
C  TRANSFORMATION OF SHAPE FUNCTIONS DERIVATIVES
C
      do 20 ig=1,ng
         do 30 k=1,10
C
C  First Derivatives
C
            tp(1)=tjaci(1,1)*tshag(k,2,ig)+tjaci(1,2)*tshag(k,3,ig)
            tp(2)=tjaci(2,1)*tshag(k,2,ig)+tjaci(2,2)*tshag(k,3,ig)
            tshagn(k,2,ig)=tp(1)
            tshagn(k,3,ig)=tp(2)
C
C  Second Derivatives
C
            tp(1)=t2j(1,1)*tshag(k,4,ig)+t2j(1,2)*tshag(k,5,ig)
     &            +t2j(1,3)*tshag(k,6,ig)
            tp(2)=t2j(2,1)*tshag(k,4,ig)+t2j(2,2)*tshag(k,5,ig)
     &            +t2j(2,3)*tshag(k,6,ig)
            tp(3)=t2j(3,1)*tshag(k,4,ig)+t2j(3,2)*tshag(k,5,ig)
     &            +t2j(3,3)*tshag(k,6,ig)
            tshagn(k,4,ig)=tp(1)
            tshagn(k,5,ig)=tp(2)
            tshagn(k,6,ig)=tp(3)
 30      continue
 20   continue
C
C  SUB-ELEMENTARY MATRIX FOR NORM SPLINE PROBLEM
C
      do 100 i=1,10
         do 110 j=1,10
            do 120 ig=1,ng
C
C  Second derivative terms
C
              tkse(i,j)=tkse(i,j)+wg(ig)*(tshagn(i,4,ig)*tshagn(j,4,ig)
     &                                   +tshagn(i,5,ig)*tshagn(j,5,ig)
     &                              +deux*tshagn(i,6,ig)*tshagn(j,6,ig))
C
C  First derivative terms
C
              tkse(i,j)=tkse(i,j)+wg(ig)*alpha1
     &                        /RLREL/RLREL*
     &                            (tshagn(i,2,ig)*tshagn(j,2,ig)
     &                            +tshagn(i,3,ig)*tshagn(j,3,ig))
C
C  Non derivated term
C
              tkse(i,j)=tkse(i,j)+wg(ig)*alpha0
     &                         /(RLREL*RLREL*RLREL*RLREL)*
     &                            (tshag(i,1,ig)*tshag(j,1,ig))
C
C  If advection constraint
C
              if(itcs.eq.1.or.itcs.eq.3) then
                 tkse(i,j)=tkse(i,j)+wg(ig)*wc1
     &                   /(RLREL*RLREL)*    
     &                   (
     &                     (u*tshagn(i,2,ig)+v*tshagn(i,3,ig)-
     &                      visc*tshagn(i,4,ig)-visc*tshagn(i,5,ig)
C Added linear sink JMB2012
     &                     +decayrate*tshag(i,1,ig)
     &                     )*
     &                     (u*tshagn(j,2,ig)+v*tshagn(j,3,ig)-
     &                      visc*tshagn(j,4,ig)-visc*tshagn(j,5,ig)
C Added linear sink JMB2012
     &                     +decayrate*tshag(j,1,ig)
     &                     )
     &                    )
              endif
 120       continue
           tkse(i,j)=tkse(i,j)*detj*stiff
 110    continue
 100  continue
C
C  CONTRIBUTION OF THE DATA POINTS
C
      ifirst=kindt(iel)+1
      if(iel.eq.nelt) then
         ilast=ndatl
                      else
         ilast=kindt(iel+1)
      endif
      do 150 id=ifirst,ilast
         idata=kdata(id)
         if(kelos(idata,1).ne.iel) then
            write(6,*)' %%% ERROR -cksel2a : CHECK DATA SORTING %%% '
         write(6,*) 'iel',iel,ifirst,ilast,kdata(id),id,kelos(idata,1)
            stop
         endif
         isel=kelos(idata,2)
         if(isel.ne.isub) goto 150
         xo=tdata(idata,1)
         yo=tdata(idata,2)
         do=tdata(idata,3)
CJMBJMB big test
         tdata(idata,4)=tdata(idata,4)/RLREL/RLREL
         wo=tdata(idata,4)
         if(ipr.ge.3) then
             write(6,151)xo,yo,do,wo
         endif
 151     format(t2,'Data Contribution: xo=',f8.2,';xo=',f8.2,';do=',
     &          f8.2,';wo=',f8.2)
C
C Transformation of the data position in reference element
C

         xi=tjaci(1,1)*(xo-x0)+tjaci(2,1)*(yo-y0)
         eta=tjaci(1,2)*(xo-x0)+tjaci(2,2)*(yo-y0)
         call ep2(xi,eta,ep)
C
C  CONTRIBUTION FROM OBSERVATIONS
C
         do 160 i=1,10
            tgse(i)=tgse(i)+wo*do*ep(i)*stiff*stiff
c     &                       /(RLREL*RLREL)
            do 170 j=1,10
               tkse(i,j)=tkse(i,j)+wo*ep(i)*ep(j)*stiff*stiff
c     &                       /(RLREL*RLREL)
 170        continue
 160     continue
 150  continue
 

CJMB2012 now add all sources; sorting has been done as for date
C Contribution from source terms here
C  If advection constraint
C
              if(itcs.eq.1.or.itcs.eq.3) then 
              if (NSOURCESLOC.GT.0) then
C Copy loop on data but with sources and make sum over all gaussian points (so take part of construction of K
C add parameters to call O

      ifirstQ=kindtQ(iel)+1
      if(iel.eq.nelt) then
         ilastQ=NSOURCESLOC
                      else
         ilastQ=kindtQ(iel+1)
      endif
C     write(6,*), iel,ifirstQ,ilastQ
      do 5150 id=ifirstQ,ilastQ
         idataQ=kdataQ(id)
C         write(6,*) idataQ,kelosQ(idataQ,1),kelosQ(idataQ,2)
         if(kelosQ(idataQ,1).ne.iel) then
            write(6,*)' %%% ERROR -cksel2b : CHECK SOURCE SORTING %%% '
            write(6,*) idataQ,kelosQ(idataQ,1),kelosQ(idataQ,2)
            stop
         endif
         iselQ=kelosQ(idataQ,2)
C         write(6,*) '??',iselQ,iel,idataQ
         if(iselQ.ne.isub) goto 5150
         
C       write(6,*) 'GE',tgse       
       do 5100 i=1,10
C sources are distributed as constant over the element 
C so Q divided by surface 
         do 5120 ig=1,ng
         tgse(i)=tgse(i)+wg(ig)*wc1
     &                   /(RLREL*RLREL)     
     &                    * (u*tshagn(i,2,ig)+v*tshagn(i,3,ig)-
     &                      visc*tshagn(i,4,ig)-visc*tshagn(i,5,ig)
C Added linear sink JMB2012
     &                     +decayrate*tshag(i,1,ig)
     &                     )*detj*stiff
     &           *2/detj*2*tdataQ(idataQ,3)

 5120    continue           
 5100  continue
              write(6,*) 'Adding sources',NSOURCESLOC,  iel,iselQ,
     &    tdataQ(idataQ,3),detj,wg(ng)         
C      write(6,*) 'GEa',tgse
 5150  continue
              endif 
              endif
 
      IF(IPR.GE.6) then
         WRITE (6,407) iel,isub
      endif
 407  FORMAT(///,T10,'SUB-ELEMENT PRE-MATRIX FOR EL ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.6) CALL IMPMAT(tkse,10,10,10,6)
      IF(IPR.GE.6) then
         WRITE (6,408) iel,isub
      endif
 408  FORMAT(///,T10,'SUB-RHS PRE-VECTOR FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.6) then
         do 200 i=1,10
            write(6,*) tgse(i)
 200     continue
      endif
C
C  CALCULATION OF DERIVATIVES AT INTERIOR INTERFACES (FOR SUBSEQUENT
C  IDENTIFICATION OF NORMAL DERIVATIVES, in ckele2)
C
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
C
C  TRANSFORMATION OF CONNECTORS FROM REFERENCE TO GLOBAL AXES SYSTEM
C  TR IS THE TRANSFORMATION MATRIX
C
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
 707  FORMAT(///,T10,'TRANSFORM. MATRIX FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.6) CALL IMPMAT(tr,10,10,10,6)
C
C TRANSFORMATION OF TKSE
C
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
C
C TRANSFORMATION OF TGSE
C
      do 650 i=1,10
         gtemp(i)=zero
         do 660 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*tgse(k)
 660     continue
 650  continue
      do 670 i=1,10
         tgse(i)=gtemp(i)
 670  continue
C
C TRANSFORMATION OF DERX1
C
      do 750 i=1,10
         gtemp(i)=zero
         do 760 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*derx1(k)
 760     continue
 750  continue
      do 770 i=1,10
         derx1(i)=gtemp(i)
 770  continue
C
C TRANSFORMATION OF DERX2
C
      do 850 i=1,10
         gtemp(i)=zero
         do 860 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*derx2(k)
 860     continue
 850  continue
      do 870 i=1,10
         derx2(i)=gtemp(i)
 870  continue
C
C TRANSFORMATION OF DERY1
C
      do 950 i=1,10
         gtemp(i)=zero
         do 960 k=1,10
            gtemp(i)=gtemp(i)+tr(k,i)*dery1(k)
 960     continue
 950  continue
      do 970 i=1,10
         dery1(i)=gtemp(i)
 970  continue
C
C TRANSFORMATION OF DERY2
C
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
 507  FORMAT(///,T10,'SUB-ELEMENT MATRIX FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.5) CALL IMPMAT(tkse,10,10,10,6)
      IF(IPR.GE.5) then
         WRITE (6,508) iel,isub
      endif
 508  FORMAT(///,T10,'SUB-RHS VECTOR FOR ELEMENT ',I5,', SUB',I5,
     &       ///)
      IF(IPR.GE.5) then
         do 201 i=1,10
            write(6,*) tgse(i)
 201     continue
      endif
      return
      end



      subroutine fixbcd(tuppe,tlowe,tdiag,trhsg,klink,kskyh,
     &                  tcndi,kcndi)
C
C  FIX BOUNDARY CONDITIONS IN THE GLOBAL MATRIX SYSTEM
C
      include'divapre.h'
      include'divainc.h'
      dimension tuppe(nterm),tlowe(*),tdiag(nddlt),trhsg(nddlt),
     &          tcndi(ncond),kcndi(ncond,info),kskyh(nddlt+1),
     &          klink(nnt)
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
      end




      subroutine impsol(tcoog,klink,sol)
C
C  PRINT THE SOLUTION AT PRINCIPAL CONNECTORS
C
      include'divapre.h'
      include'divainc.h'
      dimension tcoog(nnt1,2),klink(nnt),sol(nddlt)
      if(ityp.eq.2.or.ityp.eq.3) then
c         write(81,400)
         do 10 i=1,nnt1
            x=tcoog(i,1)
            y=tcoog(i,2)
            val=sol(klink(i))
            valx=sol(klink(i)+1)
            valy=sol(klink(i)+2)
            write(81,401)x,y,val,valx,valy
 10      continue
      endif
 400  format(72('&'),/,t8,'X',t18,'Y',t29,'PHI',t44,'PHIX',t59,'PHIY',
     &       /,72('&'))
 401  format(t3,f8.3,t13,f8.3,t26,f10.5,t41,f10.5,t56,f10.5)
      return
      end




      subroutine locse2 (klocs)
C
C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 2 (FDV ELEMENT)
C
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
      end



      subroutine locse3 (klocs)
C
C  INTEGRATE ELEMENTARY MATRIX WHEN ITYP = 3 (FDV ELEMENT)
C
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
      end

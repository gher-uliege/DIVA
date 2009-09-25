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
      END




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
      END


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
      END




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
      END

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
      END

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
      END

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
      END

    
      subroutine JMBCONSTRL(U,X,Y,II,icord,valexll)
C,dxkm,dykm)
      include 'divapre.h'
      include 'divainc.h'
      real*8 U,X,Y
      real*4 valexll

      real*4 x0f,dxxf,dyxf,y0f,dxyf,dyyf,xt,yt,UT,VT

      integer II,imaxf,jmaxf,icord,itwo,ia
      integer imaxff,jmaxff
c      parameter(imaxff=500,jmaxff=500)
c      real*4 UF(imaxff,jmaxff),VF(imaxff,jmaxff)
      real*8 xx,yy,dxkm,dykm
      COMMON/JMBCONL/x0f,dxxf,dyxf,y0f,dxyf,dyyf,UF,VF,imaxf,jmaxf,
     &  ierrl
C23456
      U=1
      
      if(II.EQ.0) then
      
      
      ierrl=0
C implement matrix reading
C Use units 91 92 (U,V) and 93(grid info)
      read(95,*,end=922,err=922) x0f
      read(95,*,end=922,err=922) y0f
      read(95,*,end=922,err=922) dxxf
      read(95,*,end=922,err=922) dyyf
      read(95,*,end=922,err=922) imaxf
      read(95,*,end=922,err=922) jmaxf
      x0f=x0f-dxxf
      y0f=y0f-dyyf
C
C
c      if(imaxf*jmaxf.gt.(imaxff*jmaxff)) then
c      write(6,*) 'increase imaxff*jmaxff to', imaxf*jmaxf
c      goto 922
c      endif
C Use allody and save pointer (to use allody with negative values afterwards?)
C 
      NSWSP=imaxf*jmaxf
C Try to read in simple precision in two fields (eq 1 double precision)
      
      ia=NSWSP
      itwo=2
      NSWSP=NSWSP/2+mod(ia,itwo)
      call allody(NSWSP,1,'WORKS',IPWSP,1)
      vrepl=1
      call uur(94,S(IPWSP),imaxf,jmaxf,valexll,vrepl)
C      call uur(92,S(IPWSP+NSWSP/2),imaxf,jmaxf)
C      call uur(91,UF,imaxf,jmaxf)
C      call uur(92,VF,imaxf,jmaxf)
      goto 923
 922  continue
      write(6,*) 'Error reading grid info for variable L '
      write(6,*) 'Assuming relative length scale=1'
      ierrl=1
      U=1
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
      if (ierrl.eq.1) then
      U=1
      return
      endif
      xt=X
      yt=Y
       
       call bilininl(S(IPWSP),
     &  x0f,dxxf,dyxf,y0f,dxyf,dyyf
     &                      ,imaxf,jmaxf,
     &                     UT,xt,yt)
      U=UT
      
C      write(6,*) 'X,Y',x,y,UT,VT
C 
      return
      endif
      end
      
      

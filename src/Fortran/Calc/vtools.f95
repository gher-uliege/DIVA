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
      end


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

      end


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
      end


      function fst(x,y,x1,y1,x2,y2)
      include'divapre.h'
      dx=x2-x1
!c      write(6,*) dx
      if(abs(dx).le.(0.0001)) then
         fst=x-x1-(dx/(y2-y1))*(y-y1)
         return
      endif
      fst=y-y1-((y2-y1)/dx)*(x-x1)
      return
      end



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
      END

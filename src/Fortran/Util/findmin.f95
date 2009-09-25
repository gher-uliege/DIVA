       parameter (nm=1000)
       real*4 x(nm),y(nm)
       i=0
       needs=0
       write(6,*) 'Searching for a minimum from'
 1     continue
       read(11,*,end=99) xx,dd,ww
       write(6,*) xx,dd,ww
       i=i+1
       x(i)=log10(xx)
       y(i)=dd
       goto 1
 99    continue
       n=i
       do i=2,n
       if(x(i).lt.x(i-1)) needs=1
       enddo
       if (needs.eq.1) then
       write(6,*) 'added sorting here'
       call QS2I1R (x,y,N)
       endif
       imin=1
       dmin=y(1)
       do i=2,n
       if (y(i).lt.dmin) then
       dmin=y(i)
       imin=i
       endif
       enddo
       if (n.le.2) then
       sn=10**(x(imin))
       write(12,*) 'S/N'
       write(12,*) sn
       write(12,*) 'VARBAK'
       write(12,*) sn/(1+sn)*ww
       write(6,*) 'S/N, VARBAK', sn,sn/(1+sn)*ww
       
       stop
       endif
       
       
       if (imin.eq.1) then
       x1=x(imin)
       y1=y(imin)
       x2=x(imin+1)
       y2=y(imin+1)
       x3=x(imin+2)
       y3=y(imin+2)
       endif
       if (imin.eq.n) then
       x1=x(imin-2)
       y1=y(imin-2)
       x2=x(imin-1)
       y2=y(imin-1)
       x3=x(imin)
       y3=y(imin)
       endif
       if ((imin.gt.1).and.(imin.lt.n)) then
       x1=x(imin-1)
       y1=y(imin-1)
       x2=x(imin)
       y2=y(imin)
       x3=x(imin+1)
       y3=y(imin+1)
       endif
       w3=y3/(x3-x1)/(x3-x2)
       w2=y2/(x2-x1)/(x2-x3)
       w1=y1/(x1-x2)/(x1-x3)
       xmin=0.5*((x1+x2)*w3+(x2+x3)*w1+(x1+x3)*w2)/(w1+w2+w3)
       if(imin.eq.1) xmin=x(imin)
       if(imin.eq.n) xmin=x(imin)
       sn=10**(xmin)
       write(12,*) 'S/N'
       write(12,*) sn
       write(12,*) 'VARBAK'
       write(12,*) sn/(1+sn)*ww
       write(6,*) 'S/N, VARBAK', sn,sn/(1+sn)*ww
       
       stop
       end
       
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

!C     .. Scalar Arguments ..
      INTEGER N
!C     .. Array Arguments ..
      real*4 IA(N), JA(N)
!C     .. Local Scalars ..
      REAL R 
      REAL*4  IIT, IT,  JJT, JT
      INTEGER IJ
!C     .. Local Arrays ..
      INTEGER IL(21), IU(21),I,J,K,NN,L,M
      

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
      END

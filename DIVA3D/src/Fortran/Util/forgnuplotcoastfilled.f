      integer nmax,ncmax
      parameter(nmax=20000,ncmax=1000)
      
      

C 
      real*4 x(nmax,ncmax),y(nmax,ncmax),lc,lcm
      real*8 surf(ncmax),asurf(ncmax)
      integer*4 iplace(ncmax)
      integer ip(ncmax),n,i
      integer ncr(nmax,4),isin(ncmax,ncmax)
      real rmin
      common/rminval/rmin
      
C read the n contours (maximum number ncmax)
      read(66,*) n
      if(n.gt.ncmax) then
      write(6,*) 'Sorry, please increase ncmax'
      stop
      endif
      do i=1,n
       read(66,*) ip(i)
       if(ip(i).gt.nmax) then
       write(6,*) 'Plotting error, please increase nmax'
       stop 'Plotting error, please increase nmax'
       
       endif
       j=1
 17     continue
       if(j.gt.ip(i)) goto 101
       read(66,*) x(j,i),y(j,i)
	    j=j+1
	    goto 17
 101    continue
      
      enddo
      
      close(10)
C

C Need to think about closing or not contours... and/or last segment..
C if not closed add point do close for easier calculation
      eps=0.000001
      xmin=x(1,1)
      xmax=x(1,1)
      ymin=y(1,1)
      ymax=y(1,1)
      do i=1,n
      isclosed=0
      if (abs(x(ip(i),i)-x(1,i)).lt.(eps*abs(x(ip(i),i)+x(1,i)))) then
      if (abs(y(ip(i),i)-y(1,i)).lt.(eps*abs(y(ip(i),i)+y(1,i)))) then
      isclosed=1
      endif
      endif
C if not closed, add point
      if(isclosed.eq.0) then
c      write(6,*) 'Closing contour ',i,' for convencience'
c      write(6,*) ip(i), ' points originally'
      ip(i)=ip(i)+1
      endif
C make sure it is closed
      x(ip(i),i)=x(1,i)
      y(ip(i),i)=y(1,i)
      enddo

  
       AREA=0
       do i=1,n
        surf(i)=0
        iplace(i)=i
        do j=1,ip(i)-1
        xmin=min(xmin,x(j,i))
        xmax=max(xmax,x(j,i))
        ymin=min(ymin,y(j,i))
        ymax=max(ymax,y(j,i))

         surf(i)=surf(i)+(y(j+1,i)-y(j,i))*(x(j+1,i)+x(j,i))
        enddo
        AREA=AREA+surf(i)
c        write(6,*) 'Signed surface of contour ',i, ': ',surf(i)/2 
        asurf(i)=abs(surf(i))
       enddo
c       write(6,*) 'Total signed surface ',AREA/2,' (+: anticlockwise)'
       call QS2I1R(asurf,iplace,n)
       
       open(46,file='dvcoastlinetailer')
        xmin=xmin-0.01*(xmax-xmin)
        xmax=xmax+0.01*(xmax-xmin)
        ymin=ymin-0.01*(ymax-ymin)
        ymax=ymax+0.01*(ymax-ymin)
        
        
        write(67,*) xmin,ymin
        write(67,*) xmax,ymin
        write(67,*) xmax,ymax
        write(67,*) xmin,ymax
        
        write(67,*)
        write(67,*)
        write(46,'(A,I3,A,I2,A)') 'plot ''fort.67'' index',
     &     0, ' using 1:2 with filledcurves ls ', 2,' t ''''  \\'
       
       
       do k=n,1,-1
        do ii=1,ip(iplace(k))-1
        write(67,*) x(ii,iplace(k)),y(ii,iplace(k))
        enddo
        write(67,*)
        write(67,*)
        icol=1
        if(surf(iplace(k)).lt.0) icol=2
        if(k.eq.1) then
        write(46,'(A,I5,A,I2,A)') ',''fort.67'' index',
     & n-k+1, ' using 1:2 with filledcurves ls ', icol,' title '''' '
        endif
        if(k.gt.1) then
        write(46,'(A,I5,A,I2,A)') ',''fort.67'' index',
     &n-k+1, ' using 1:2 with filledcurves ls ', icol,' title '''' \\'
        endif
       enddo
       stop
       end
       
       
        
      subroutine QS2I1R (IA,JA,N)
C=============================================================================
C *** DESCRIPTION (from www.netlib.org)
C     Written by Rondall E Jones
C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
C     algorithm. date 18 November 1976.
C
C     Further modified by David K. Kahaner
C     National Bureau of Standards
C     August, 1981
C
C     Even further modification made to bring the code up to the
C     Fortran 77 level and make it more readable and to carry
C     along one integer array and one real array during the sort by
C     Mark K. Seager
C     Lawrence Livermore National Laboratory
C     November, 1987
C     This routine was adapted from the ISORT routine.
C
C     ABSTRACT
C         This routine sorts an  array IA and makes the same
C         interchanges in the integer array JA 
C         The array IA may be sorted in increasing order
C         A slightly modified quicksort algorithm is used.
C
C     DESCRIPTION OF PARAMETERS
C        IA -  array of values to be sorted.
C        JA - Integer array to be carried along.
C     
C         N - Number of values in integer array IA to be sorted.

C     .. Scalar Arguments ..
      implicit none
      INTEGER N
C     .. Array Arguments ..
      real*8  IA(N) 
      integer*4 JA(N)
C     .. Local Scalars ..
      REAL*4 R 
      INTEGER*4 I,  IJ,  J, JJT, JT, K, L, M,NN
      REAL*8   IIT,  IT
C     .. Local Arrays ..
      INTEGER*4 IL(21), IU(21)

C --- FIRST EXECUTABLE STATEMENT  QS2I1R ---
      NN=N
      if (N.EQ.1) then
      write(6,*) 'No need to sort a single data point'
      return
      endif

C     Sort IA and carry JA and A along.
C     And now...Just a little black magic...
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

C     Select a central element of the array and save it in location
C     it, jt, at.
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)

C     If first element of array is greater than it, interchange with it.
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
      ENDIF
      L=J

C     If last element of array is less than it, swap with it.
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)

C     If first element of array is greater than it, swap with it.
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
         ENDIF
      ENDIF

C     Find an element in the second half of the array which is
C     smaller than it.
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240

C     Find an element in the first half of the array which is
C     greater than it.
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245

C     Interchange these elements.
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         GOTO 240
      ENDIF

C     Save upper and lower subscripts of the array yet to be sorted.
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

C     Begin again on another portion of the unsorted array.
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
C=============================================================================
      END




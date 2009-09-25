       integer imax
       parameter(imax=1000000)
       real*8 s(imax),sa(imax)
       real*4 dd(imax),dad(imax),ed(imax)
       real*4 xd(imax),yd(imax)
       integer*4 iw(imax)
       real*8 smean,svar,mad,med
       n=0
       no=0
       eps=0.00001
       valex=-9999.
!c      read(13,*,end=123)xori,yori
!c         read(13,*)dx,dy
!c         read(13,*)nx,ny
!c         read(13,*)valex
       read(5,*) valex      
       nnn=0
 1     continue
       read(44,*,end=9999) x,y,d
       read(71,*,end=9999) xa,ya,da
       read(76,*,end=9999) xe,ye,e
       
       nnn=nnn+1
       
       
       if( (abs(x-xe).gt.eps*abs(x)).or.(abs(x-xa).gt.eps*abs(x)).or.(abs(y-ye).gt.eps*abs(y)).or.(abs(y-ya).gt.eps*abs(y)) ) then
       write(6,*) 'Incoherent files'
       write(6,*) 'difference found in record',nnn
       stop
       endif
!c       if (abs(da-valex).le.eps*abs(valex)) goto 1
!c       if (abs(e-valex).le.eps*abs(valex)) goto 1
       
       n=n+1
       if(n.gt.imax) stop 'increase imax'
       s(n)=(d-da)/e
       sa(n)=abs(s(n))
       iw(n)=n
       dd(n)=d
       dad(n)=da
       ed(n)=e
       xd(n)=x
       yd(n)=y
       if (abs(da-valex).le.eps*abs(valex)) then
       s(n)=0
       sa(n)=abs(s(n))
       dd(n)=d
       dad(n)=d
       ed(n)=10000+dd(n)
       endif
!C sort the outliers?
       goto 1
 9999  continue
       if (n.eq.0) stop 'Problem in input?'
       call QS2I1R(sa,iw,n)
       do i=n,1,-1
       if (abs(dd(iw(i))-dad(iw(i))).gt.2*ed(iw(i))) then
       if (abs(dd(iw(i))-dad(iw(i))).gt.3*ed(iw(i))) then
!c        write(66,*) xd(i),yd(i),dd(i),dad(i),
!c     & ed(i),abs(dd(i)-dad(i))/ed(i),iw(i)
        
        write(66,1234) sa(i),iw(i),xd(iw(i)),yd(iw(i)),dd(iw(i)), dad(iw(i)),ed(iw(i))

     
            endif
        no=no+1
       endif
!C       write(6,*) 'Test passed',x,y,d,da,e
!C       goto 1
!C 123   continue
       enddo
  123  continue 
       do i=1,n
       iw(i)=i
       enddo
 
 
       if(no.gt.(0.05*n)) then
       write(6,*) 'There are more outliers than usual :',no,' out of',n
       write(66,*) 'There are more outliers than usual :',no,' out of',n
       endif
       if (no.eq.0) then
       write(66,*) 'There are no outliers'
       write(6,*) 'There are no outliers'
       endif
       if(no.le.(0.05*n)) then
       write(6,*) 'There are a usual number of outliers',no,' out of',n
       write(66,*) 'There are a usual number of outliers at 2 s:',no,' out of',n
       
       endif
       write(66,*) 'Points with value of first column larger than 3 are suspect, if there are more than ', n*0.003, ' of them'
!c normalized test, rather then mean and variance, med and mad
       call madmed(s,mad,med,n,iw)
       
       
       do i=n,1,-1
       if (s(i).gt.3*mad) then
!c       write(6,*) s(i)/mad,iw(i),xd(iw(i)),yd(iw(i)),dd(iw(i))
        write(67,1234) s(i)/mad,iw(i),xd(iw(i)),yd(iw(i)),dd(iw(i)) , dad(iw(i)),ed(iw(i))
       endif
       enddo
       write(67,*) 'Points with value of first column larger than 3 are suspect, if there are more than ', n*0.003, ' of them'
       
       write(6,*) 'relative biais in misfits is',med
       write(66,*) 'relative biais in misfits is',med
       write(67,*) 'relative biais in misfits is',med
       write(6,*) 'normalized variance should be one but is ',mad
       write(66,*) 'normalized variance should be one but is',mad
       write(67,*) 'normalized variance should be one but is',mad
 1234  format(1X,1E10.4,1X,1I8,5(1X,1E10.4))
       stop
       end
       
       subroutine median(s,med,n,iw)
       real*8 s(n),med
       integer*4 iw(n)
       call QS2I1R(s,iw,n)
       if (mod(n,2).eq.0) then
       med=(s(n/2)+s(n/2+1))/2.
       else
       med=s((n+1)/2)
       endif
       return
       end
       
       subroutine madmed(s,mad,med,n,iw)
       
       real*8 s(n),med,mad
       integer*4 iw(n)
       
       call median(s,med,n,iw)
       
       do i=1,n
       s(i)=abs(s(i)-med)
       enddo
       
       call median(s,mad,n,iw)
       mad=mad*1.4826
       
       return
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
!C         This routine sorts an  array IA and makes the same
!C         interchanges in the integer array JA
!C         The array IA may be sorted in increasing order
!C         A slightly modified quicksort algorithm is used.
!C
!C     DESCRIPTION OF PARAMETERS
!C        IA -  array of values to be sorted.
!C        JA - Integer array to be carried along.
!C
!C         N - Number of values in integer array IA to be sorted.

!C     .. Scalar Arguments ..
      implicit none
      INTEGER N
!C     .. Array Arguments ..
      real*8  IA(N) 
      integer*4 JA(N)
!C     .. Local Scalars ..
      REAL*4 R 
      INTEGER*4 I,  IJ,  J, JJT, JT, K, L, M,NN
      REAL*8   IIT,  IT
!C     .. Local Arrays ..
      INTEGER*4 IL(21), IU(21)

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




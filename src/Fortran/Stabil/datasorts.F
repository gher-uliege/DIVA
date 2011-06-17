************************************************************************
      PROGRAM DATASORTING
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,LEVELS,nblines,ndsort
      INTEGER :: fnum,lev,first_level,last_level,itrans
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL,y,
     &                                       dinvnorm,ys
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: x,temp
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
      CHARACTER (len=99)   :: ALLDATA
!
      CHARACTER (len=99)                :: VARFILEIN,VARFILEOU
      CHARACTER (len=255)               :: file_name,INVERFILE
      CHARACTER (len=99)                :: var_name
!
      LOGICAL                           :: exist
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) itrans
      READ(44,*) first_level
      READ(44,*) last_level
      READ(44,*) nblines
      CLOSE(44)

      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(y(nblines+2))
      ALLOCATE(ys(nblines+2))
      ALLOCATE(dinvnorm(nblines+2))

!
        WRITE(VARFILEIN,
     &'(a,"_noise")')TRIM(var_name)
!
       OPEN(10, FILE=VARFILEIN, STATUS='OLD')
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
         ENDDO
       CLOSE(10)
!
       DO n = 1,nblines
         i=n
         x=MINVAL(y(n:nblines))
         DO WHILE(y(i) .gt. x)
          i=i+1
         ENDDO
         temp=y(n)
         y(n)=y(i)
         y(i)=temp
       ENDDO
!
       WRITE(VARFILEOU,
     &'(a,"_sorted00")')TRIM(var_name)
       WRITE(ALLDATA,
     &'("../input/divadata/",a)')TRIM(VARFILEOU)
       OPEN(UNIT=11,FILE=ALLDATA,STATUS='NEW')

       DO n = 1,nblines
           write(11,*) y(n)
       ENDDO
       CLOSE(11)
!
       i=0
       DO n = 1,nblines-1
        IF(y(n) .lt. y(n+1)) then
         i=i+1
         ys(i)=y(n)
        ENDIF
       ENDDO
       IF(y(nblines-1) .lt. y(nblines)) then
       i=i+1
       ys(i) = y(nblines)
       ENDIF
       ndsort = i
       ys(ndsort+1)=ys(ndsort)+(ys(ndsort)-ys(ndsort-1))
       ndsort = ndsort+ 1
       ys(ndsort+1)=ys(ndsort)+(ys(ndsort)-ys(ndsort-1))
       ndsort = ndsort+ 1
!
       write(*,*)'% ndsort nblines ',ndsort,nblines
!
       WRITE(VARFILEOU,
     &'(a,"_sorted")')TRIM(var_name)
       WRITE(ALLDATA,
     &'("../input/divadata/",a)')TRIM(VARFILEOU)
       OPEN(UNIT=11,FILE=ALLDATA,STATUS='NEW')

       DO n = 1,ndsort !nblines
           write(11,*) ys(n)
       ENDDO
       CLOSE(11)
!
        WRITE(VARFILEOU,
     &'(a,"_inverf")')TRIM(var_name)
        WRITE(INVERFILE,
     &'("../input/divadata/",a)')TRIM(VARFILEOU)
!
       CALL INVERFW(nblines,ndsort,ys,dinvnorm) !(nblines,y,dinvnorm)
!
       OPEN(10, FILE=INVERFILE, STATUS='NEW')
       DO n = 1,ndsort !nblines
           write(10,*) ys(n),dinvnorm(n)
       ENDDO
       CLOSE(10)
      write(*,*)'data sorting: OK'
      STOP
  999 continue
      write(*,*)'data sorting: '
      write(*,*)'total number of data does not match for '
      write(*,*)TRIM(ALLDATA)
      STOP
!
!-----Done--------------------------------------------------------------
!
      END
!
      SUBROUTINE INVERFW(nblignes,ndsort,don,dinvnorm)
!
c http://home.online.no/~pjacklam/notes/invnorm
c a routine written by john herrero
! adapted version for GHER use in DIVA software, Jan 2011
!
      implicit none

      real, parameter :: a1=-39.6968302866538
      real, parameter :: a2=220.946098424521

      integer              :: ndsort,nblignes,i
      real*8               :: p,p_low,p_high
      real*8               :: b1,b2,b3,b4,b5
      real*8               :: c1,c2,c3,c4,c5,c6
      real*8               :: d1,d2,d3,d4
      real*8               :: z,q,r,interval
      real*8               :: donnee,test
      real*8               :: a3,a4,a5,a6
      REAL, DIMENSION(nblignes) :: don,dinvnorm
!
      interval=((1-2*0.00009))/(nblignes)

      p=0
      test=0
      do i = 1,ndsort
         p=0.00009+(i-1)*interval
         test=p+0.01
c    Algorithme de calcul de erfinv
c    ------------------------------
      a3=-275.928510446969
      a4=138.357751867269
      a5=-30.6647980661472
      a6=2.50662827745924
      b1=-54.4760987982241
      b2=161.585836858041
      b3=-155.698979859887
      b4=66.8013118877197
      b5=-13.2806815528857
      c1=-0.00778489400243029
      c2=-0.322396458041136
      c3=-2.40075827716184
      c4=-2.54973253934373
      c5=4.37466414146497
      c6=2.93816398269878
      d1=0.00778469570904146
      d2=0.32246712907004
      d3=2.445134137143
      d4=3.75440866190742
      p_low=0.02425
      p_high=1-p_low
      if(p.lt.p_low) goto 201
      if(p.ge.p_low) goto 301
201   q=dsqrt(-2*dlog(p))
      z=(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/
     &((((d1*q+d2)*q+d3)*q+d4)*q+1)
      goto 204
301   if((p.ge.p_low).and.(p.le.p_high)) goto 202
      if(p.gt.p_high) goto 302
202   q=p-0.5
      r=q*q
      z=(((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q/
     &(((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
      goto 204
302   if((p.gt.p_high).and.(p.lt.1)) goto 203
203   q=dsqrt(-2*dlog(1.d0-p))
      z=-(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/
     &((((d1*q+d2)*q+d3)*q+d4)*q+1)
204   dinvnorm(i)=z

      enddo
      continue
      end


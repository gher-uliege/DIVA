!
      PROGRAM MINMAXVA
!
!
      implicit none
!
!------------------------------------------------------------
!
      REAL :: rmin,rmax,parval
!
      CHARACTER (len=8)                :: VARFILEIN,VARFILOUT
!------------------------------------------------------------
!
      VARFILEIN = 'MinMaxVa'
      VARFILOUT = 'ParamVal'
!
      OPEN(2,FILE=VARFILEIN,STATUS='OLD')
         READ(2,*) rmin
         READ(2,*) rmax
         READ(2,*) parval
      CLOSE(2)
      IF(parval .le. rmin) parval = rmin
      IF(parval .ge. rmax) parval = rmax
      OPEN(3,FILE=VARFILOUT)
         WRITE(3,*) parval
      CLOSE(3)
!
      stop
      end


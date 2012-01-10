!
      PROGRAM WCLFILE
!
!
      implicit none
!
!------------------------------------------------------------
!
      INTEGER :: istep,MINLEV,MAXLEV
      INTEGER :: nlev
!
      REAL    :: rmin,rmax
!
      CHARACTER (len=8)                :: VARFILEIN,comments
      CHARACTER (len=15)               :: EXECIN
      CHARACTER (len=31)               :: VARFINFO
!------------------------------------------------------------
!
      VARFILEIN = 'cl.1xxxx'
      VARFINFO  = '../input/divaparam/CLminmax'
!
      EXECIN='../input/3Dinfo'
      OPEN(2,FILE=EXECIN,STATUS='OLD')
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*)! comments
      READ(2,*) MINLEV
      READ(2,*)! comments
      READ(2,*) MAXLEV
      CLOSE(2)
!
      OPEN(2,FILE=VARFINFO,STATUS='OLD')
      DO nlev = 1,MINLEV-1
         READ(2,*) rmin,rmax
      ENDDO
      DO istep = MINLEV,MAXLEV
         READ(2,*) rmin,rmax
         WRITE(VARFILEIN(5:8),'(I4.4)') istep
         OPEN(3,FILE=VARFILEIN)
         WRITE(3,*) rmin
         WRITE(3,*) rmax
         CLOSE(3)
      ENDDO
      CLOSE(2)
!
      stop
      end


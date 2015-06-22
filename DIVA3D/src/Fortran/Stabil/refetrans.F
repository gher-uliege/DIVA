************************************************************************
      PROGRAM REFETRANS
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbccol,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans
      INTEGER :: nbval
      integer :: NX, NY
      integer*4                 :: IMAX, JMAX,kmax,IPR
!
      REAL, DIMENSION(:), ALLOCATABLE :: LONVAL,LATVAL,VARVAL,VECFLD,U
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght
      REAL*4 :: VALEXC
      real*4 :: xorig, yorig, dx, dy, xend, yend
      real*8                           :: W8
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
      CHARACTER (len=256) :: datafile2
      CHARACTER (len=20)   :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO,VARFILEOU
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name
!
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) itrans
      READ(44,*) first_level
      READ(44,*) last_level
      READ(44,*) nblines
      READ(44,*) nbccol
      READ(44,*) istep
      CLOSE(44)

      divafile = '../input/divarefe/GridInfo.dat'
      open(unit=90,file=divafile)
      read(90,*) xorig
      read(90,*) yorig
      read(90,*) dx
      read(90,*) dy
      read(90,*) xend
      read(90,*) yend
      read(90,*) VALEXC
      CLOSE(90)
      NX = INT(xend)
      NY = INT(yend)
      ALLOCATE(U(NX*NY))
!
      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(VECFLD(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
        WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep

        WRITE(datafile,
     &'("../input/divarefe/",a,".datapoint.ref")')TRIM(VARFILEIN)

       OPEN(10, FILE=TRIM(datafile), STATUS='OLD')
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ELSE
         write(*,*)' refetrans: column number /= 3 for file '
         write(*,*) TRIM(datafile)
         STOP 'stoped'
       ENDIF
       CLOSE(10)

!------------------------------------------------
! Sylvain Watelet - 22/06/2015
! Handling valex
!------------------------------------------------
      nbval = 0
      VALEXC=-99.
      DO i=1,nblines
          IF(VARVAL(i) .ne. VALEXC) THEN
           nbval = nbval+1
           VECFLD(nbval) = VARVAL(i)
          ENDIF
      ENDDO
!
         CALL TRANSFUN(itrans,nbval,VECFLD) !nblines,VARVAL
!
      nbval = 0
       DO i=1,nblines
        IF(VARVAL(i) .ne. VALEXC) THEN
           nbval = nbval+1
           VARVAL(i) = VECFLD(nbval)
        ENDIF
      ENDDO
!------------------------------------------------
!
       OPEN(11, FILE=TRIM(datafile))
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
!	write(0,*) varval(n)
         ENDDO
!	call sleep(10)

       CLOSE(11)
       STOP
  999 continue
      write(*,*)'total number of data does not match for '
      write(*,*)TRIM(datafile)
!
      STOP
!
!-----Done--------------------------------------------------------------
!
      END





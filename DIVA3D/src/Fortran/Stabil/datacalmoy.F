************************************************************************
      PROGRAM DATACALMOY
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbccol,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans
!
      REAL, DIMENSION(:),  ALLOCATABLE  :: LONVAL,LATVAL,VARVAL
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght,DATAMOY
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
      CHARACTER (len=20)   :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO,VARFILEOU
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name

      LOGICAL                          :: exist
!
!-----------------------------------------------------------------------
!
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) nblines
      READ(44,*) nbccol
      READ(44,*) istep
      CLOSE(44)

      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
        WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep
        WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
!
       OPEN(10, FILE=datafile, STATUS='OLD')
       IF(nbccol == 10) THEN
!
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,FINLINE(1,n)
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
       CLOSE(10)
!
      DATAMOY = 0.
      DO n = 1,nblines
      DATAMOY = DATAMOY + VARVAL(n)
      ENDDO
      DATAMOY = DATAMOY/nblines
!!!!!       DATAMOY = SUM(VARVAL(1:nblines))/nblines
!
        WRITE(VARFILEIN,
     &'("MOY_",a,".dat")')TRIM(var_name)
        WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
!
      INQUIRE(FILE=datafile,EXIST=exist)
      IF(exist) then
       OPEN(10, FILE=datafile, STATUS='OLD',POSITION='APPEND')
      ELSE
       OPEN(10, FILE=datafile, STATUS='NEW')
      ENDIF
      write(10,*) DATAMOY
      CLOSE(10)
      STOP
!
  999 continue
      write(*,*)'datacamoy:'
      write(*,*)'total number of data does not match for '
      write(*,*)TRIM(datafile)
!
      STOP
!
!-----Done--------------------------------------------------------------
!
      END





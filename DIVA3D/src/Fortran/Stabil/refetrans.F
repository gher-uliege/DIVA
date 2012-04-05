************************************************************************
      PROGRAM REFETRANS
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbccol,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght
!
      CHARACTER (len=256) :: divafile,datafile,infodata,comments
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
!
      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
        WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep

        WRITE(datafile,
     &'("../input/divarefe/",a,".datapoint.ref")')TRIM(VARFILEIN)
!
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
!
         CALL TRANSFUN(itrans,nblines,VARVAL)
!
       OPEN(11, FILE=TRIM(datafile))
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO

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





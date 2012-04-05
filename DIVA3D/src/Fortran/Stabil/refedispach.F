************************************************************************
      PROGRAM REFEDISPACH
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,j,ivar, istep,LEVELS,nbccol,nblines,nbcols,nblnall
      INTEGER :: fnum,lev,first_level,last_level,itrans,idum,nbfiles
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: yvar,ynf

      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL,y
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght,ran2
!
      CHARACTER (len=256) :: divafile,datafile,dummyf,buitn
      CHARACTER (len=99)   :: ALLDATA,INVERFILE
!
      CHARACTER (len=99)                :: VARFILEIN,VARFINFO,VARFILEOU
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name
!
      LOGICAL                           :: exist
!-----------------------------------------------------------------------

!
      OPEN(33, FILE='fort.33')
      READ(33,*) var_name
      READ(33,*) itrans
      READ(33,*) first_level
      READ(33,*) last_level
      READ(33,*) nblnall
      READ(33,*) nbfiles
      CLOSE(33)
!
      ALLOCATE(yvar(nblnall))
      ALLOCATE(ynf(nblnall))
!
        WRITE(VARFILEOU,
     &'(a,"_inverf")')TRIM(var_name)
        WRITE(INVERFILE,
     &'("../input/divarefe/",a)')TRIM(VARFILEOU)
!
       OPEN(10, FILE=INVERFILE, STATUS='OLD')
       DO n = 1,nblnall
           read(10,*) yvar(n),ynf(n)

       ENDDO
       CLOSE(10)
!
      open(unit=20,file='refbruitls')
      open(unit=21,file='lsdivarefe')

      DO m =  1,nbfiles

      read(20,*) dummyf
      read(21,*) datafile,nblines,nbccol

      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(y(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
        WRITE(file_name,
     &'("../input/divarefe/",a)')TRIM(dummyf)
!
       OPEN(10, FILE=TRIM(file_name), STATUS='OLD')

       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
         ENDDO
       ELSE
         write(*,*)' refedispach: column number /= 3 for file '
         write(*,*) TRIM(file_name)
         STOP 'stoped'
       ENDIF
       CLOSE(10)
!
      DO n = 1,nblines
      j=1
      IF(y(n) .EQ. yvar(j)) THEN
          VARVAL(n) = ynf(j)
      ELSE
          DO while(y(n) .NE. yvar(j))
            j=j+1
          ENDDO
          VARVAL(n) = ynf(j)
      ENDIF
      ENDDO
!
        WRITE(file_name,
     &'("../input/divarefe/",a)')TRIM(datafile)
!
       OPEN(UNIT=10,FILE=file_name)
       DO n = 1,nblines
         write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
       ENDDO
       CLOSE(10)

      DEALLOCATE(LONVAL)
      DEALLOCATE(LATVAL)
      DEALLOCATE(VARVAL)
      DEALLOCATE(y)
      DEALLOCATE(FINLINE)    

      ENDDO

      STOP
  999 continue
      write(*,*)'data dispching: '
      write(*,*)'total number of data does not match for '
      write(*,*)TRIM(dummyf)
!
      STOP
!
!-----Done--------------------------------------------------------------
!
      END

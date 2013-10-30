************************************************************************
      PROGRAM DATADISPACH
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
      OPEN(44, FILE='fort.44')
      READ(44,*) var_name
      READ(44,*) itrans
      READ(44,*) first_level
      READ(44,*) last_level
      READ(44,*) nblnall
      READ(44,*) nbfiles
      CLOSE(44)
!
      ALLOCATE(yvar(nblnall))
      ALLOCATE(ynf(nblnall))
!
        WRITE(VARFILEOU,
     &'(a,"_inverf")')TRIM(var_name)
        WRITE(INVERFILE,
     &'("../input/divadata/",a)')TRIM(VARFILEOU)
!
       OPEN(10, FILE=INVERFILE, STATUS='OLD')
       DO n = 1,nblnall
           read(10,*) yvar(n),ynf(n)

       ENDDO
       CLOSE(10)
!
      open(unit=20,file='bruitls')
      open(unit=21,file='lsdivadata')

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
     &'("../input/divadata/",a)')TRIM(dummyf)
!
       OPEN(10, FILE=file_name, STATUS='OLD')
       IF(nbccol == 10) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
     &,FINLINE(1,n)
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n),y(n)
         ENDDO
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
     &'("../input/divadata/",a)')TRIM(datafile)
!
       OPEN(UNIT=10,FILE=file_name)
       IF(nbccol == 10) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n))
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           write(10,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
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

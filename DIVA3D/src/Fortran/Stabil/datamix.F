************************************************************************
      PROGRAM DATAMIX
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,ic1,ic2,nbccol,nblines
     &,nbcols,nbcolm,nbcolp,nblinep,nblinem
     &,nbccols,nbccolm,nbccolp
      INTEGER :: fnum,lev,first_level,last_level,itrans,iplus,iminus
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL,WVAL
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght
!
      CHARACTER (len=256) :: divafile,infodata,comments
      CHARACTER (len=256) :: datafile,datafilep,datafilem
      CHARACTER (len=20)   :: EXECIN 
!
      CHARACTER (len=99)                :: VARFILEIN,VARFILEP,VARFILEM
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
      READ(44,*) nblines,nblinep,nblinem
      READ(44,*) nbccol,nbccolp,nbccolm
      READ(44,*) istep
      CLOSE(44)
      nbcols=max(1,nbccol-3)
      ALLOCATE(LONVAL(nblines))
      ALLOCATE(LATVAL(nblines))
      ALLOCATE(VARVAL(nblines))
      ALLOCATE(WVAL(nblines))
      ALLOCATE(FINLINE(nbcols,nblines))    
!
      IF((last_level - first_level) .gt. 2) THEN
        ic1=0
        IF(istep == first_level) THEN
           iplus = istep+2
           iminus = istep+1
        ELSE
           IF(istep == last_level) THEN
             iplus = istep-1
             iminus = istep-2
           ELSE
             iplus = istep+1
             iminus = istep-1
           ENDIF
        ENDIF
      ELSE
        ic1=1
        IF(istep == first_level) THEN
           iplus = last_level
        ELSE
           IF(istep == last_level) THEN
             iplus = first_level
           ENDIF
        ENDIF
      ENDIF
          
      WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep
      WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
          
      WRITE(VARFILEP,
     &'(a,".1",i4.4)')TRIM(var_name),iplus
      WRITE(datafilep,
     &'("../input/divadata/",a)')TRIM(VARFILEP)

      IF(ic1 == 0) THEN           
       WRITE(VARFILEM,
     &'(a,".1",i4.4)')TRIM(var_name),iminus
       WRITE(datafilem,
     &'("../input/divadata/",a)')TRIM(VARFILEM)
      ENDIF
!
      OPEN(10, FILE=datafile, STATUS='OLD')
!
       FINLINE='00'
!
       IF(nbccol .ge. 10) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
            read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
       CLOSE(10)
!
       WVAL=itrans*WVAL
!
       OPEN(11, FILE=datafile)

       IF(nbccol == 10) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF

      DEALLOCATE(LONVAL)
      DEALLOCATE(LATVAL)
      DEALLOCATE(VARVAL)
      DEALLOCATE(WVAL)
      DEALLOCATE(FINLINE)
!
      IF(ic1 == 0) THEN           

      nbcolm=max(1,nbccolm-3)
      ALLOCATE(LONVAL(nblinem))
      ALLOCATE(LATVAL(nblinem))
      ALLOCATE(VARVAL(nblinem))
      ALLOCATE(WVAL(nblinem))
      ALLOCATE(FINLINE(nbcolm,nblinem))    

      OPEN(10, FILE=datafilem, STATUS='OLD')
!
       IF(nbccolm .ge. 10) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccolm == 9) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccolm == 8) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccolm == 7) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccolm == 6) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccolm == 5) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccolm == 4) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccolm == 3) THEN
         DO n = 1,nblinem
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
       CLOSE(10)
!
       IF(nbccolm == 10) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccolm == 9) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccolm == 8) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccolm == 7) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccolm == 6) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccolm == 5) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccolm == 4) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccolm == 3) THEN
         DO n = 1,nblinem
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF

      DEALLOCATE(LONVAL)
      DEALLOCATE(LATVAL)
      DEALLOCATE(VARVAL)
      DEALLOCATE(WVAL)
      DEALLOCATE(FINLINE)

       ENDIF

!=======================
      nbcolp=max(1,nbccolp-3)
      ALLOCATE(LONVAL(nblinep))
      ALLOCATE(LATVAL(nblinep))
      ALLOCATE(VARVAL(nblinep))
      ALLOCATE(WVAL(nblinep))
      ALLOCATE(FINLINE(nbcolp,nblinep))    

      OPEN(10, FILE=datafilep, STATUS='OLD')
!
       IF(nbccolp .ge. 10) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
         ENDDO
       ENDIF
       IF(nbccolp == 9) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n),FINLINE(6,n)
         ENDDO
       ENDIF
       IF(nbccolp == 8) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &,FINLINE(5,n)
         ENDDO
       ENDIF
       IF(nbccolp == 7) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
         ENDDO
       ENDIF
       IF(nbccolp == 6) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n),FINLINE(3,n)
         ENDDO
       ENDIF
       IF(nbccolp == 5) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),FINLINE(2,n)
         ENDDO
       ENDIF
       IF(nbccolp == 4) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccolp == 3) THEN
         DO n = 1,nblinep
           read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF
       CLOSE(10)
!
       IF(nbccolp == 10) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccolp == 9) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccolp == 8) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccolp == 7) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccolp == 6) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccolp == 5) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccolp == 4) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,WVAL(n)
         ENDDO
       ENDIF
       IF(nbccolp == 3) THEN
         DO n = 1,nblinep
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
         ENDDO
       ENDIF

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





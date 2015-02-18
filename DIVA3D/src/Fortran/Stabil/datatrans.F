************************************************************************
      PROGRAM DATATRANS
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbccol,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL*4 :: lon,lat,val, wght, minv
      INTEGER, DIMENSION(1) :: loc
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
      READ(44,'(a)') var_name
      READ(44,*) itrans
      READ(44,*) first_level
      READ(44,*) last_level
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

      Write(*,*) 'DATATRANS start',itrans

        WRITE(VARFILEIN,
     &'(a,".1",i4.4)')TRIM(var_name),istep
        WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(VARFILEIN)
!
       OPEN(10, FILE=datafile, STATUS='OLD')
       IF(nbccol == 10) THEN
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

      IF(itrans .eq. 11 .or. itrans .eq. 21) then

         loc(1:1) = MINLOC(VARVAL(1:nblines))
         i=loc(1)
         minv = VARVAL(i)

         IF(minv .lt. 0.) THEN

           WRITE(*,*) '============: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
           WRITE(*,*) '!!! WARNING : found negative values in data set '
           WRITE(*,*) '============: data minval =',minv
           WRITE(*,*) '============: Will eliminate negative data and  '
           WRITE(*,*) '============: go for log transf '
           WRITE(*,*) '============: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

            OPEN(11, FILE=datafile)
            m = 0

            IF(nbccol == 10) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                    ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &                    ,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 9) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                    ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &                    ,' ',TRIM(FINLINE(6,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 8) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                    ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 7) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                    ,' ',TRIM(FINLINE(4,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 6) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 5) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &                    ,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 4) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &                    ,TRIM(FINLINE(1,n))
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            IF(nbccol == 3) THEN
               DO n = 1,nblines
                  IF(VARVAL(n) .ge. 0.) then
                     write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
                     m = m + 1
                  ENDIF
               ENDDO
            ENDIF
            CLOSE(11)
            nblines = m

         ENDIF   
!
         DEALLOCATE(LONVAL)
         DEALLOCATE(LATVAL)
         DEALLOCATE(VARVAL)
         DEALLOCATE(FINLINE)
!
         ALLOCATE(LONVAL(nblines))
         ALLOCATE(LATVAL(nblines))
         ALLOCATE(VARVAL(nblines))
         ALLOCATE(FINLINE(nbcols,nblines))    
!
         OPEN(10, FILE=datafile, STATUS='OLD')
         IF(nbccol == 10) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &              ,FINLINE(5,n),FINLINE(6,n),FINLINE(7,n)
            ENDDO
         ENDIF
         IF(nbccol == 9) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &              ,FINLINE(5,n),FINLINE(6,n)
            ENDDO
         ENDIF
         IF(nbccol == 8) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
     &              ,FINLINE(5,n)
            ENDDO
         ENDIF
         IF(nbccol == 7) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n),FINLINE(4,n)
            ENDDO
         ENDIF
         IF(nbccol == 6) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n),FINLINE(3,n)
            ENDDO
         ENDIF
         IF(nbccol == 5) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n),FINLINE(2,n)
            ENDDO
         ENDIF
         IF(nbccol == 4) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &              ,FINLINE(1,n)
            ENDDO
         ENDIF
         IF(nbccol == 3) THEN
            DO n = 1,nblines
               read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
            ENDDO
         ENDIF
         CLOSE(10)
!
      ENDIF
!
      CALL TRANSFUN(itrans,nblines,VARVAL)
!
         OPEN(11, FILE=datafile)
       IF(nbccol == 10) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
         ENDDO
       ENDIF
       IF(nbccol == 9) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &,' ',TRIM(FINLINE(6,n))
         ENDDO
       ENDIF
       IF(nbccol == 8) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
         ENDDO
       ENDIF
       IF(nbccol == 7) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &,' ',TRIM(FINLINE(4,n))
         ENDDO
       ENDIF
       IF(nbccol == 6) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
         ENDDO
       ENDIF
       IF(nbccol == 5) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n))
         ENDDO
       ENDIF
       IF(nbccol == 4) THEN
         DO n = 1,nblines
           write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n))
         ENDDO
       ENDIF
       IF(nbccol == 3) THEN
         DO n = 1,nblines
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





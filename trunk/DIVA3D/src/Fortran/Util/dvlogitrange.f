************************************************************************
      Programm LogItRange
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines
      REAL    :: VAL, minv,maxv,A,B, T1, T2
      REAL, DIMENSION(:), ALLOCATABLE :: VARVAL
      INTEGER, DIMENSION(1) :: loc
!
      CHARACTER (len=255)               :: file_name
      CHARACTER (len=99)                :: var_name
!
!read total data number
      OPEN(UNIT=11,FILE='all_nblall')
      read(11,*) nblines
      read(11,*) var_name
      CLOSE(11)

      ALLOCATE(VARVAL(nblines))

!read user range (if provided)
      OPEN(UNIT=11,FILE='Logitrange')
      read(11,*) A
      read(11,*) B
      CLOSE(11)

!read all data values
      OPEN(UNIT=11,FILE='ALL_DATA')
      DO n = 1,nblines
         read(11,*) VAL,VAL,VARVAL(n)
      ENDDO
      CLOSE(11)

      loc(1:1) = MINLOC(VARVAL(1:nblines))
      i=loc(1)
      minv = VARVAL(i)

      loc(1:1) = MAXLOC(VARVAL(1:nblines))
      i=loc(1)
      maxv = VARVAL(i)

      IF( A .lt. B) then

         T1 = A - 0.1*(maxv - minv)
         T2 = B + 0.1*(maxv - minv)
         IF( minv .gt. T1 .AND. maxv .lt. T2) THEN
            WRITE(*,*) ' Logit transformation: data are in range',A,B
            WRITE(*,*) ' ---------------------'
         ELSE
            WRITE(*,*) ' Logit transformation: WARNING !!!!!!!!!'
            WRITE(*,*) ' !!!!!!!!!!!!!!!!! data are not in range',A,B
         IF( A .ge. minv .OR. B .le. maxv) then
!           WRITE(*,*) 'Logit transformation: data are NOT in range',A,B
           WRITE(*,*) '        AND will be clipped to fall into range'
            CALL DVDATASHRINK(A,B)
         ENDIF
         ENDIF
!
         open(unit=10,file='logitAB')
         write(10,*) A
         write(10,*) B
         close(10)
      ELSE
         WRITE(*,*) ' Logit transformation: using min and max as range'
     &,minv,maxv
         WRITE(*,*) ' ---------------------'
         open(unit=10,file='logitAB')
         write(10,*) minv
         write(10,*) maxv
         close(10)
      ENDIF
!
!!      RETURN
      END

      SUBROUTINE DVDATASHRINK(A,B)
!
      IMPLICIT NONE
!
      INTEGER :: n,m,i,ivar, istep,LEVELS,nbcol3,nblines,nbcols
      INTEGER :: fnum,lev,first_level,last_level,itrans
!
      REAL, DIMENSION(:),    ALLOCATABLE  :: LONVAL,LATVAL,VARVAL
      CHARACTER(len=256), DIMENSION(:,:),ALLOCATABLE :: COLLINE,FINLINE
      REAL :: A,B
!
      CHARACTER (len=256)  :: datafile
      CHARACTER (len=255)  :: file_name
      CHARACTER (len=99)   :: var_name
!

!read file info
      OPEN(UNIT=11,FILE='all_nblall')
      read(11,*) nblines
      read(11,*) var_name
      read(11,*) first_level
      read(11,*) last_level
      CLOSE(11)

      DO i = first_level,last_level
         istep = 10000 + i
         WRITE(file_name,
     &'(a,"_1",i4.4,"_info")')TRIM(var_name),istep

         OPEN(UNIT=11,FILE=file_name)
         read(11,*) nblines
         read(11,*) nbcols
         read(11,*) lev
         CLOSE(11)

         IF(istep /= 10000+lev) Then
            WRITE(*,*) ' DVDATASHRINK: istep /= lev',istep
            STOP ' dvlogitrange: check dvdatatransf???'
         ENDIF
!
         nbcol3=max(1,nbcols-3)
         ALLOCATE(LONVAL(nblines))
         ALLOCATE(LATVAL(nblines))
         ALLOCATE(VARVAL(nblines))
         ALLOCATE(FINLINE(nbcols-3,nblines))

         WRITE(file_name,
     &'(a,".1",i4.4)')TRIM(var_name),istep
         WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(file_name)
         OPEN(10, FILE=datafile, STATUS='OLD')
         DO n = 1,nblines
            read(10,*,end=999) LONVAL(n),LATVAL(n),VARVAL(n)
     &                          ,FINLINE(1:nbcol3,n)
         ENDDO
         CLOSE(10)

         WRITE(file_name,
     &'(a,".1",i4.4,"_shrink")')TRIM(var_name),istep
         WRITE(datafile,
     &'("../input/divadata/",a)')TRIM(file_name)
         OPEN(11, FILE=datafile, STATUS='NEW')

         IF(nbcols == 10) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                 ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &                 ,' ',TRIM(FINLINE(6,n)),' ',TRIM(FINLINE(7,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 9) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                 ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
     &                 ,' ',TRIM(FINLINE(6,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 8) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                 ,' ',TRIM(FINLINE(4,n)),' ',TRIM(FINLINE(5,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 7) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
     &                 ,' ',TRIM(FINLINE(4,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 6) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n)),' ',TRIM(FINLINE(3,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 5) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &                 ,TRIM(FINLINE(1,n)),' ',TRIM(FINLINE(2,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 4) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
     &                 ,TRIM(FINLINE(1,n))
               ENDIF
            ENDDO
         ENDIF
         IF(nbcols == 3) THEN
            DO n = 1,nblines
               IF(VARVAL(n) .ge. A .AND. VARVAL(n) .le. B) THEN
                  write(11,*) LONVAL(n),LATVAL(n),VARVAL(n)
               ENDIF
            ENDDO
         ENDIF
         CLOSE(11)
      ENDDO
      WRITE(*,*) 'DVDATASHRINK: Finished shrinking'
      RETURN
 999  WRITE(*,*) 'DVDATASHRINK: Problem while reading',datafile
      STOP 'dvlogitrange: Problem while reading'
      END

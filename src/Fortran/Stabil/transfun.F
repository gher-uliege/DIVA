************************************************************************
      SUBROUTINE TRANSFUN(itrans,nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans
      REAL    :: VAL,x
      REAL, DIMENSION(nblines) :: VARVAL
!
      IF(itrans .eq. 14 .or. itrans .eq. 24) then
         CALL USERFUN(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 12 .or. itrans .eq. 22) then
!xx         DO n = 1,nblines
!xx          VAL = (VARVAL(n))**2
!xx          VARVAL(n) = VAL
!xx         ENDDO

           WRITE(*,*) 'No transformation for flag ',itrans

         RETURN
      ENDIF
      IF(itrans .eq. 11 .or. itrans .eq. 21) then
         x=MINVAL(VARVAL(1:nblines))
         IF(x.lt.0.) THEN
           write(55,*) 'trans not possible'
           WRITE(*,*) 'SEVER ERROR: trans not possible for flag ',itrans
           WRITE(*,*) '============ data minval =',x
           STOP
         ENDIF
         DO n = 1,nblines
            IF(VARVAL(n) == 0.) VARVAL(n) = 1.e-14
          VAL = log(VARVAL(n))
          VARVAL(n) = VAL
         ENDDO
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
!xx         DO n = 1,nblines
!xx          VAL = exp(VARVAL(n))
!xx          VARVAL(n) = VAL
!xx         ENDDO

           WRITE(*,*) 'No transformation for flag ',itrans

      ENDIF
!
      RETURN
      END
!
************************************************************************
      SUBROUTINE TRANSINV(itrans,nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans,nblninv,l
      REAL    :: VAL
!      REAL, DIMENSION(:),    ALLOCATABLE  :: VARVAL
      REAL, DIMENSION(nblines) :: VARVAL
      INTEGER, DIMENSION(2)   :: loc

!      ALLOCATE(VARVAL(nblines)
!
      IF(itrans .eq. 14 .or. itrans .eq. 24) then
         CALL USERINV(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 12 .or. itrans .eq. 22) then
!xx         DO n = 1,nblines
!xx          VAL = sqrt(VARVAL(n))
!xx          VARVAL(n) = VAL
!xx         ENDDO

           WRITE(*,*) 'No transformation for flag ',itrans

         RETURN
      ENDIF
      IF(itrans .eq. 11 .or. itrans .eq. 21) then
         DO n = 1,nblines
          VAL = exp(VARVAL(n))
          VARVAL(n) = VAL
         ENDDO
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
!xx         DO n = 1,nblines
!xx          VAL = log(VARVAL(n))
!xx          VARVAL(n) = VAL
!xx         ENDDO

           WRITE(*,*) 'No transformation for flag ',itrans

      ENDIF
!
      RETURN
      END

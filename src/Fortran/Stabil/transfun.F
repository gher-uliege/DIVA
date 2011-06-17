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
         DO n = 1,nblines
          VAL = (VARVAL(n))**2
          VARVAL(n) = VAL
         ENDDO
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
          VAL = log(VARVAL(n)+1.e-4)
          VARVAL(n) = VAL
         ENDDO
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
         DO n = 1,nblines
          VAL = exp(VARVAL(n))
          VARVAL(n) = VAL
         ENDDO
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
      INTEGER :: n,i,nblines,itrans,nblninv
      REAL    :: VAL
!      REAL, DIMENSION(:),    ALLOCATABLE  :: VARVAL
      REAL, DIMENSION(nblines) :: VARVAL
!      ALLOCATE(VARVAL(nblines)
!
      IF(itrans .eq. 14 .or. itrans .eq. 24) then
         CALL USERINV(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 12 .or. itrans .eq. 22) then
         DO n = 1,nblines
          VAL = sqrt(VARVAL(n))
          VARVAL(n) = VAL
         ENDDO
         RETURN
      ENDIF
      IF(itrans .eq. 11 .or. itrans .eq. 21) then
         DO n = 1,nblines
          VAL = exp(VARVAL(n))-1.e-4
          VARVAL(n) = VAL
         ENDDO
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
         DO n = 1,nblines
          VAL = log(VARVAL(n))
          VARVAL(n) = VAL
         ENDDO
      ENDIF
!
      RETURN
      END

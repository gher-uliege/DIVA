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
         CALL LOGITTRANS(nblines,VARVAL)
         RETURN
      ENDIF

      IF(itrans .eq. 11 .or. itrans .eq. 21) then
         CALL LOGNTRANS(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
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
         CALL LOGITINVTA(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 11 .or. itrans .eq. 21) then
         CALL LOGNINVTA(nblines,VARVAL)
         RETURN
      ENDIF
      IF(itrans .eq. 10 .or. itrans .eq. 20) then
           WRITE(*,*) 'No transformation for flag ',itrans
      ENDIF
!
      RETURN
      END

************************************************************************
      SUBROUTINE LOGITTRANS(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans
      REAL    :: VAL,x, minv,maxv,sig2,A,B, ksy, F0
      REAL, DIMENSION(nblines) :: VARVAL
      INTEGER, DIMENSION(1) :: loc

         open(unit=10,file='logitAB',STATUS='OLD')
         read(10,*) minv
         read(10,*) maxv
         close(10)
!
         ksy = - A/(B-A)
         IF(ksy .le. 0.) ksy = 1.e-5
         F0 = log(ksy/(1.-ksy))

         A = minv - 0.01*(maxv-minv)
         B = maxv + 0.01*(maxv-minv)

         DO n = 1,nblines
           ksy = (VARVAL(n)- A)/(B-A)
           VAL = log(ksy/(1.-ksy))
           VARVAL(n) = VAL - F0
         ENDDO

      RETURN
      END

************************************************************************
      SUBROUTINE LOGITINVTA(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans
      REAL    :: VAL,x, minv,maxv,sig2,A,B, ksy, F0
      REAL, DIMENSION(nblines) :: VARVAL
      INTEGER, DIMENSION(1) :: loc

      open(unit=10,file='logitAB',STATUS='OLD')
      read(10,*) minv
      read(10,*) maxv
      close(10)

      ksy = - A/(B-A)
      IF(ksy .le. 0.) ksy = 1.e-5
      F0 = log(ksy/(1.-ksy))

      A = minv
      B = maxv

      DO n = 1,nblines
           ksy = 1./(1.+ exp(-(VARVAL(n) + F0)))
           VAL = A + ksy*(B - A)
           VARVAL(n) = VAL
      ENDDO

      RETURN
      END

************************************************************************
      SUBROUTINE LOGNTRANS(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans
      REAL    :: VAL,x, minv,maxv,sig2,A,B, ksy, F0
      REAL, DIMENSION(nblines) :: VARVAL
      INTEGER, DIMENSION(1) :: loc

      IF(nblines .gt. 1) then
         sig2 = SUM((VARVAL(:)**2))-(SUM(VARVAL(:))**2/nblines)
         sig2 = sig2 / (nblines-1)
      ELSE
         sig2 = VARVAL(1)
      ENDIF

         F0 = log(1.e-3)

         DO n = 1,nblines
          IF(VARVAL(n) == 0.) VARVAL(n) = 1.e-3*sig2
          VAL = log(VARVAL(n))
          VARVAL(n) = VAL - F0
         ENDDO

      RETURN
      END

************************************************************************
      SUBROUTINE LOGNINVTA(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines,itrans
      REAL    :: VAL,x, minv,maxv,sig2,A,B, ksy, F0
      REAL, DIMENSION(nblines) :: VARVAL
      INTEGER, DIMENSION(1) :: loc
!
      F0 = log(1.e-3)

      DO n = 1,nblines
          VAL = exp(VARVAL(n) + F0)
          VARVAL(n) = VAL
      ENDDO

      RETURN
      END

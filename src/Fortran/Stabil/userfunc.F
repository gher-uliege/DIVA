************************************************************************
      SUBROUTINE USERFUN(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines
      REAL    :: VAL
!      REAL, DIMENSION(:),    ALLOCATABLE  :: VARVAL
      REAL, DIMENSION(nblines) :: VARVAL
!      ALLOCATE(VARVAL(nblines)

!      DO n = 1,nblines
!        VAL = my_transformation(VARVAL(n))
!        VARVAL(n) = VAL
!      ENDDO

      RETURN
      END

************************************************************************
      SUBROUTINE USERINV(nblines,VARVAL)
************************************************************************
!
      IMPLICIT NONE
!
      INTEGER :: n,i,nblines
      REAL    :: VAL
!      REAL, DIMENSION(:),    ALLOCATABLE  :: VARVAL
      REAL, DIMENSION(nblines) :: VARVAL
!      ALLOCATE(VARVAL(nblines)

!      DO n = 1,nblines
!        VAL =  my_inverse_transformation(VARVAL(n))
!        VARVAL(n) = VAL
!      ENDDO
      RETURN
      END

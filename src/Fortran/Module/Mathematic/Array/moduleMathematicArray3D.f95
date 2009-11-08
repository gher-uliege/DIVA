MODULE moduleMathematicArray3D

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module specifications               ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================


! Include file
! ============
   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, memoryGetFirstIndexY, memoryGetFirstIndexZ, &
                                           memoryGetLastIndexX, memoryGetLastIndexY, memoryGetLastIndexZ
   USE moduleValuesArrayManagement, ONLY : memoryGetValues

   INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
#ifdef _REAL_
   PUBLIC :: mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
             mathArrayScale
#endif

   PUBLIC :: mathArrayMin, mathArrayMax, mathArrayAbsMin, mathArrayAbsMax

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

#ifdef _REAL_
! Procedure 1 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION mathArrayNorm1() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         val = val + abs(ptr(i1,i2,i3))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathArrayNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      val = zero
      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         ptr => ptr1(i1,i2,i3)
         val = val + ptr * ptr
        END DO
       END DO
      END DO

      val = sqrt(val)

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathArrayNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         val = max(val,abs(ptr(i1,i2,i3)))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathArraySum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         val = val + ptr(i1,i2,i3)
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathArraySqrt()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         ptr => ptr1(i1,i2,i3)
         ptr = sqrt(abs(ptr))
        END DO
       END DO
      END DO

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE mathArrayScale(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr1 => memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
        DO i3 = istartZ, iendZ
         ptr => ptr1(i1,i2,i3)
         ptr = val * ptr
        END DO
       END DO
      END DO

  END SUBROUTINE

#endif

! Procedure 7 : min value
! -----------------------
  FUNCTION mathArrayMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = min(val,ptr(i1,i2,i3))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 8 : max value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = max(val,ptr(i1,i2,i3))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 9 : min value
! -----------------------
  FUNCTION mathArrayAbsMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = min(val,abs(ptr(i1,i2,i3)))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 10 : max value
! -----------------------
  FUNCTION mathArrayAbsMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = max(val,abs(ptr(i1,i2,i3)))
        END DO
       END DO
      END DO

  END FUNCTION

END MODULE moduleMathematicArray3D

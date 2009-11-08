MODULE moduleMathematicArray2D

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
   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, memoryGetFirstIndexY,  &
                                           memoryGetLastIndexX, memoryGetLastIndexY,  &
                                           memoryGetValues

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
             mathArrayScale, mathArrayMin, mathArrayMax, mathArrayAbsMin, mathArrayAbsMax


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

! Procedure 1 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION mathArrayNorm1() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         val = val + abs(ptr(i1,i2))
       END DO
      END DO

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathArrayNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      val = zero
      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         ptr => ptr1(i1,i2)
         val = val + ptr * ptr
       END DO
      END DO

      val = sqrt(val)

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathArrayNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         val = max(val,abs(ptr(i1,i2)))
       END DO
      END DO

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathArraySum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         val = val + ptr(i1,i2)
       END DO
      END DO

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathArraySqrt()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         ptr => ptr1(i1,i2)
         ptr = sqrt(abs(ptr))
       END DO
      END DO

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE mathArrayScale(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr1 => memoryGetValues()

      DO i1 = istartX, iendX
       DO i2 = istartY, iendY
         ptr => ptr1(i1,i2)
         ptr = val * ptr
       END DO
      END DO

  END SUBROUTINE

! Procedure 7 : min value
! -----------------------
  FUNCTION mathArrayMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = min(val,ptr(i1,i2))
       END DO
      END DO

  END FUNCTION

! Procedure 8 : max value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = max(val,ptr(i1,i2))
       END DO
      END DO

  END FUNCTION

! Procedure 9 : min value
! -----------------------
  FUNCTION mathArrayAbsMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = min(val,abs(ptr(i1,i2)))
       END DO
      END DO

  END FUNCTION

! Procedure 10 : max value
! -----------------------
  FUNCTION mathArrayAbsMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = max(val,abs(ptr(i1,i2)))
       END DO
      END DO

  END FUNCTION

END MODULE moduleMathematicArray2D

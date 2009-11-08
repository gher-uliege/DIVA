MODULE moduleMathematicArray1D

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
   USE moduleArrayDefinition
   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX,   &
                                           memoryGetLastIndexX, &
                                           memoryGetSize
   USE moduleValuesArrayManagement, ONLY : memoryGetValues

   INCLUDE 'constantParameter.h'

! Declaration
! ===========

!  General part
!  ------------
#ifdef _REAL_
   TYPE (arrayType), PRIVATE, POINTER :: secondWorkingArray => NULL()
#endif

! Procedures status
! =================

!  General part
!  ------------
#ifdef _REAL_
   PUBLIC :: mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
             mathArrayScale, mathArrayDot, setSecondWorkingArray, nullifySecondArrayPointer
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
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
         val = val + abs(ptr(i1))
      END DO

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathArrayNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      val = zero
      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
         ptr => ptr1(i1)
         val = val + ptr * ptr
      END DO

      val = sqrt(val)

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathArrayNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
         val = max(val,abs(ptr(i1)))
      END DO

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathArraySum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istartX, iendX
         val = val + ptr(i1)
      END DO

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathArraySqrt()

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr1 =>  memoryGetValues()

      DO i1 = istartX, iendX
         ptr => ptr1(i1)
         ptr = sqrt(abs(ptr))
      END DO

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE mathArrayScale(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr1 => memoryGetValues()

      DO i1 = istartX, iendX
         ptr => ptr1(i1)
         ptr = val * ptr
      END DO

  END SUBROUTINE

! Procedure 7 : make the dot product of 2 array 1D
! -------------------------------------------------
  FUNCTION mathArrayDot() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, size, istart, iend, istart2
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr1
      VARType, DIMENSION(:), POINTER :: ptr2


!     Body
!     - - -
      size = memoryGetSize()
      istart = memoryGetFirstIndexX()
      iend = memoryGetLastIndexX()

      istart2 = secondWorkingArray%firstIndexX

      ptr1 => memoryGetValues()
      ptr2 => secondWorkingArray%values

      val = zero

      IF ( size /= secondWorkingArray%nbOfDataX ) RETURN

      IF ( istart == istart2 ) THEN
         DO i1 = istart, iend
             val = val + ptr2(i1) * ptr1(i1)
         END DO
      ELSE
         DO i1 = istart, iend
             val = val + ptr2(i1-istart+istart2) * ptr1(i1)
         END DO
      ENDIF

  END FUNCTION

! Procedure 8 : setting pointer to array
! ---------------------------------------
   SUBROUTINE setSecondWorkingArray(targetArray)

!     Declaration
!     - - - - - -
      TYPE(arrayType), INTENT(IN), TARGET :: targetArray

!     Body
!     - - -
     secondWorkingArray => targetArray

   END SUBROUTINE

! Procedure 9 : make the target of the pointer null
! --------------------------------------------------
   SUBROUTINE nullifySecondArrayPointer()

!     Body
!     - - -
      secondWorkingArray => NULL()

   END SUBROUTINE

#endif

! Procedure 10 : min value
! -----------------------
  FUNCTION mathArrayMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = ptr(istartX)

      DO i1 = istartX + 1, iendX
         val = min(val,ptr(i1))
      END DO

  END FUNCTION

! Procedure 11 : max value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = ptr(istartX)

      DO i1 = istartX + 1 , iendX
         val = max(val,ptr(i1))
      END DO

  END FUNCTION

! Procedure 12 : min value
! -----------------------
  FUNCTION mathArrayAbsMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = ptr(istartX)

      DO i1 = istartX + 1, iendX
         val = min(val,abs(ptr(i1)))
      END DO

  END FUNCTION

! Procedure 13 : max value
! -----------------------
  FUNCTION mathArrayAbsMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = ptr(istartX)

      DO i1 = istartX + 1 , iendX
         val = max(val,abs(ptr(i1)))
      END DO

  END FUNCTION


END MODULE moduleMathematicArray1D

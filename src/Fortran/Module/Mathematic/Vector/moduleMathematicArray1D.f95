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
   PUBLIC :: mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, &
             mathArrayScale, mathArrayDot, setSecondWorkingArray, nullifySecondArrayPointer
#endif

   PUBLIC :: mathArrayMin, mathArrayMax, mathArrayAbsMin, mathArrayAbsMax, mathArraySum


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
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = sum(abs(ptr(istartX:iendX)))

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathArrayNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = sqrt(sum(ptr(istartX:iendX)*ptr(istartX:iendX)))

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathArrayNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = maxval(abs(ptr(istartX:iendX)))

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathArraySqrt()

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr1 =>  memoryGetValues()
      ptr1(istartX:iendX) = sqrt(ptr1(istartX:iendX))

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE mathArrayScale(val)

!     Declaration
!     - - - - - -
      INTEGER ::  istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr1 => memoryGetValues()

      ptr1(istartX:iendX) = val * ptr1(istartX:iendX)

  END SUBROUTINE

! Procedure 7 : make the dot product of 2 array 1D
! -------------------------------------------------
  FUNCTION mathArrayDot() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: length, istart, iend, istart2, iend2
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr1
      VARType, DIMENSION(:), POINTER :: ptr2


!     Body
!     - - -
      length = memoryGetSize()
      istart = memoryGetFirstIndexX()
      iend = memoryGetLastIndexX()

      istart2 = secondWorkingArray%firstIndexX
      iend2 = secondWorkingArray%lastIndexX

      ptr1 => memoryGetValues()
      ptr2 => secondWorkingArray%values

      val = zero

      IF ( length /= secondWorkingArray%nbOfDataX ) RETURN

      val = dot_product(ptr1(istart:iend),ptr2(istart2:iend2))

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
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = minval(ptr(istartX:iendX))

  END FUNCTION

! Procedure 11 : max value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = ptr(istartX)

      val = maxval(ptr(istartX:iendX))

  END FUNCTION

! Procedure 12 : min value
! -----------------------
  FUNCTION mathArrayAbsMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = minval(abs(ptr(istartX:iendX)))

  END FUNCTION

! Procedure 13 : max value
! -----------------------
  FUNCTION mathArrayAbsMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = maxval(abs(ptr(istartX:iendX)))

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathArraySum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      ptr =>  memoryGetValues()

      val = sum(ptr(istartX:iendX))

  END FUNCTION


END MODULE moduleMathematicArray1D

MODULE template

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

! Include module
! ===============
   USE moduleNorm
   USE templatevectorType
   USE templateBasicVector, ONLY : &
             printInformation, vectorDestroy, vectorSetSize, vectorGetSize, vectorSetToZero, vectorSetToValue, &
             vectorMin, vectorMax, vectorInsertValue, vectorAddValue, vectorGetValue, &
             vectorCreateBase, vectorCreateWithDimension, vectorCreateWithDimensionAndStartingPoint, vectorGetValues, &
             vectorGetStartIndex, vectorGetEndIndex, &
             memoryGetSize,  memoryGetStartingPoint, memoryGetFinalValuePosition, memoryGetValues, &
             setWorkingVector, nullify

! Include file
! ============
   INCLUDE 'constantParameter.h'

! Declaration
! ===========

!  General part
!  ------------
   TYPE (vectorType), PRIVATE, POINTER :: secondWorkingVector => NULL()

! Procedures status
! =================
   PUBLIC :: vectorNorm1, vectorNorm2, vectorNormInfinity, vectorSqrt, vectorSum, vectorScale, vectorDot, vectorNorm

!  General part
!  ------------
   PRIVATE :: setSecondWorkingVector, nullifySecond

!  Mathematic part
!  ---------------
   PRIVATE ::  mathVectorNorm1, mathVectorNorm2, mathVectorNormInfinity, mathVectorSqrt, mathVectorSum, &
               mathVectorScale, mathVectorDot

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

! ============================================================
! ===            External procedure ("PUBLIC")             ===
! ============================================================

! Procedure 8 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION vectorNorm1(targetVector) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      val = mathVectorNorm1()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 9 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION vectorNorm2(targetVector) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      val = mathVectorNorm2()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 10 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION vectorNormInfinity(targetVector) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      val = mathVectorNormInfinity()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 11 : sum(xi)
! ---------------------
  FUNCTION vectorSum(targetVector) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      val = mathVectorSum()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 12 : make sqrt of all values
! -------------------------------------
  SUBROUTINE vectorSqrt(targetVector)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      CALL mathVectorSqrt()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 17 : scale the vector values
! --------------------------------------
  SUBROUTINE vectorScale(targetVector,val)

!     Declaration
!     - - - - - -
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      CALL mathVectorScale(val)
      
!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 18 : make the dot product of 2 vectors
! -------------------------------------------------
  FUNCTION vectorDot(targetVector1,targetVector2) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector1, targetVector2
      CALL setWorkingVector(targetVector1)
      CALL setSecondWorkingVector(targetVector2)

!     Body
!     - - -
      val = mathVectorDot()
      
!     Nullify pointer
!     - - - - - - - -
      CALL nullify()
      CALL nullifySecond()

  END FUNCTION

! Procedure 20 : norm
! -------------------
  FUNCTION vectorNorm(targetVector,inormType) RESULT(val)

!     Declaration
!     - - - - - -
      TYPE(normType), INTENT(IN) :: inormType
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(vectorType), INTENT(IN) :: targetVector
      CALL setWorkingVector(targetVector)

!     Body
!     - - -
      IF ( inormType == normL1 ) THEN
         val = mathVectorNorm1()
      ELSE IF ( inormType == normL2 ) THEN
         val = mathVectorNorm2()
      ELSE IF ( inormType == normInfinity ) THEN
         val = mathVectorNormInfinity()
      ELSE
         val = mathVectorNorm2()
      ENDIF

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION


! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures  : general                ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================

! Procedure 1 : setting second pointer to vector
! ----------------------------------------------
   SUBROUTINE setSecondWorkingVector(targetVector)

!     Declaration
!     - - - - - -
      TYPE(vectorType), INTENT(IN), TARGET :: targetVector

!     Body
!     - - -
      secondWorkingVector => targetVector

   END SUBROUTINE

! Procedure 2 : make the target of the second pointer null
! --------------------------------------------------------
   SUBROUTINE nullifySecond()

!     Body
!     - - -
      secondWorkingVector => NULL()

   END SUBROUTINE


! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures : mathematic              ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================


! Procedure 1 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION mathVectorNorm1() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istart, iend
         val = val + abs(ptr(i1))
      END DO

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathVectorNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      val = zero
      ptr1 =>  memoryGetValues()

      DO i1 = istart, iend
         ptr => ptr1(i1)
         val = val + ptr * ptr
      END DO
      
      val = sqrt(val)

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathVectorNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istart, iend
         val = max(val,abs(ptr(i1)))
      END DO

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathVectorSum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      val = zero
      ptr =>  memoryGetValues()

      DO i1 = istart, iend
         val = val + ptr(i1)
      END DO

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathVectorSqrt()

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      ptr1 =>  memoryGetValues()

      DO i1 = istart, iend
         ptr => ptr1(i1)
         ptr = sqrt(abs(ptr))
      END DO

  END SUBROUTINE

! Procedure 6 : scale the vector values
! -------------------------------------
  SUBROUTINE mathVectorScale(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, istart, iend
      VARType :: val
      VARType, POINTER :: ptr
      VARType, DIMENSION(:), POINTER :: ptr1

!     Body
!     - - -
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(memoryGetSize(),istart)

      ptr1 => memoryGetValues()
      
      DO i1 = istart, iend
         ptr => ptr1(i1)
         ptr = val * ptr
      END DO

  END SUBROUTINE

! Procedure 7 : make the dot product of 2 vectors
! -----------------------------------------------
  FUNCTION mathVectorDot() RESULT(val)
  
!     Declaration
!     - - - - - -
      INTEGER :: i1, size, istart, iend, istart2
      VARType :: val
      VARType, DIMENSION(:), POINTER :: ptr1
      VARType, DIMENSION(:), POINTER :: ptr2


!     Body
!     - - -
      size = memoryGetSize()
      istart = memoryGetStartingPoint()
      iend = memoryGetFinalValuePosition(size,istart)
      istart2 = secondWorkingVector%startValue

      ptr1 => memoryGetValues()
      ptr2 => secondWorkingVector%values
      
      val = zero

      IF ( size /= secondWorkingVector%nbOfData ) RETURN

      DO i1 = istart, iend
          val = val + ptr2(i1-istart+istart2) * ptr1(i1)
      END DO

  END FUNCTION

END MODULE template


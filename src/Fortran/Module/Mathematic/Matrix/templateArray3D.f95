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
   USE templatearrayType
   USE templateBasicArray, ONLY : &
             printInformation, arrayDestroy, arraySetSize, arrayGetSizeX, arrayGetSizeY, arrayGetSizeZ, arraySetToZero, &
             arraySetToValue, arrayMin, arrayMax, arrayInsertValue, arrayAddValue, arrayGetValue, &
             arrayCreateBase, arrayCreateWithDimension, arrayCreateWithDimensionAndStartingPoint, arrayGetValues, &
             arraySetIncreaseSize, &
             arrayGetStartIndexX, arrayGetEndIndexX, arrayGetStartIndexY, arrayGetEndIndexY, arrayGetStartIndexZ, &
             arrayGetEndIndexZ, memoryGetSizeX, memoryGetSizeY, memoryGetSizeZ, memoryGetStartingPointX, memoryGetStartingPointY, &
             memoryGetStartingPointZ, memoryGetFinalValuePosition, memoryGetValues, setWorkingArray, nullify

! Include file
! ============
   INCLUDE 'constantParameter.h'

! Declaration
! ===========

!  General part
!  ------------

! Procedures status
! =================
   PUBLIC :: arrayNorm1, arrayNorm2, arrayNormInfinity, arraySqrt, arraySum, arrayScale, arrayNorm

!  Mathematic part
!  ---------------
   PRIVATE ::  mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
               mathArrayScale

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

! Procedure 1 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION arrayNorm1(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayNorm1()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION arrayNorm2(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayNorm2()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION arrayNormInfinity(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayNormInfinity()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION arraySum(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArraySum()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE arraySqrt(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL mathArraySqrt()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 6 : scale the array values
! --------------------------------------
  SUBROUTINE arrayScale(targetArray,val)

!     Declaration
!     - - - - - -
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL mathArrayScale(val)
      
!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 7 : norm
! -------------------
  FUNCTION arrayNorm(targetArray,inormType) RESULT(val)

!     Declaration
!     - - - - - -
      TYPE(normType), INTENT(IN) :: inormType
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      IF ( inormType == normL1 ) THEN
         val = mathArrayNorm1()
      ELSE IF ( inormType == normL2 ) THEN
         val = mathArrayNorm2()
      ELSE IF ( inormType == normInfinity ) THEN
         val = mathArrayNormInfinity()
      ELSE
         val = mathArrayNorm2()
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
  FUNCTION mathArrayNorm1() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

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


END MODULE template


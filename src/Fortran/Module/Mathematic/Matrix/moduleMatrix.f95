MODULE moduleMatrix

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

! Preprocessing declaration
! =========================

! Include file
! ============
   USE moduleArrayDefinition
#ifdef _REAL_
   USE moduleNorm
#endif

   USE moduleArray, ONLY : arrayGetValues, arrayGetValue, arrayGetAllocationStatus, arrayGetPointerOnValue, &
                           arrayArraySetToZero, arrayArraySetToValue, arrayArrayInsertValue, arrayArrayAddValue, &
                           arrayArrayFastInsertValue, arrayArrayFastAddValue, arraySetIncreaseSize, &
                           arrayDestructor, arrayPrintInformation, arrayCreateBase, arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndex, arraySetSize, &
                           arrayGetFirstIndexX, arrayGetLastIndexX, arrayGetSizeX, arrayGetAllocatedSizeX , arrayGetIncreaseSizeX, &
                           arrayGetDefaultIncreaseSizeX, arraySetIncreaseSizeX, &
                           arrayGetFirstIndexY, arrayGetLastIndexY, arrayGetSizeY, arrayGetAllocatedSizeY , arrayGetIncreaseSizeY, &
                           arrayGetDefaultIncreaseSizeY, arraySetIncreaseSizeY


   USE moduleMathematicArray2D, ONLY : mathArrayMin, mathArrayMax, &
#ifdef _REAL_
                                       mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
                                       mathArrayScale, &
#endif
                                       mathArrayAbsMin, mathArrayAbsMax


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: arrayArrayMin, arrayArrayMax, &
#ifdef _REAL_
             arrayArrayNorm1, arrayArrayNorm2, arrayArrayNormInfinity, arrayArrayNorm, arrayArraySqrt, arrayArraySum, &
             arrayArrayScale, &
#endif
             arrayArrayAbsMin, arrayArrayAbsMax

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
  FUNCTION arrayArrayNorm1(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
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
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION arrayArrayNorm2(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
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
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION arrayArrayNormInfinity(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
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
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION arrayArraySum(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
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
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE arrayArraySqrt(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL mathArraySqrt()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE arrayArrayScale(targetArray,val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL mathArrayScale(val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 7 : compute the norm of the vector
! --------------------------------------------
  FUNCTION arrayArrayNorm(targetArray,normSelection) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(normType), INTENT(IN) :: normSelection
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      SELECT CASE (normSelection%normTypeValue)
         CASE (normL1%normTypeValue)
            val = mathArrayNorm1()
         CASE (normL2%normTypeValue)
            val = mathArrayNorm2()
         CASE (normInfinity%normTypeValue)
            val = mathArrayNormInfinity()
         CASE DEFAULT
            val = mathArrayNorm2()
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 8 : min value
! -----------------------
  FUNCTION arrayArrayMin(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayMin()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 9 : max value
! -----------------------
  FUNCTION arrayArrayMax(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayMax()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 10 : min value
! -----------------------
  FUNCTION arrayArrayAbsMin(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayAbsMin()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 11 : max value
! -----------------------
  FUNCTION arrayArrayAbsMax(targetArray) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = mathArrayAbsMax()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION


END MODULE moduleMatrix


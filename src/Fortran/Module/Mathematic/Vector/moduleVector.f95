MODULE moduleVector

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

   USE moduleWorkingArray, ONLY : setWorkingArray, nullifyArrayPointer
   USE moduleArray, ONLY : arrayGetValues, arrayGetValue, arrayGetAllocationStatus, arrayGetPointerOnValue, &
                           arrayArraySetToZero, arrayArraySetToValue, arrayArrayInsertValue, arrayArrayAddValue, &
                           arrayArrayFastInsertValue, arrayArrayFastAddValue, arraySetIncreaseSize, &
                           arrayDestructor, arrayPrintInformation, arrayCreateBase, arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndex, arraySetSize, &
                           arrayGetFirstIndex,arrayGetLastIndex, arrayGetSize, arrayGetAllocatedSize , arrayGetIncreaseSize, &
                           arrayGetDefaultIncreaseSize, &
                           arrayGetFirstIndexX, arrayGetLastIndexX, arrayGetSizeX, arrayGetAllocatedSizeX , arrayGetIncreaseSizeX, &
                           arrayGetDefaultIncreaseSizeX, arraySetIncreaseSizeX, &
                           arrayArrayMin, arrayArrayMax, arrayOptimize, arrayArraySetValue, &
#ifdef _REAL_
                           arrayArrayNorm1, arrayArrayNorm2, arrayArrayNormInfinity, arrayArrayNorm, arrayArraySqrt, &
                           arrayArrayScale, &
                           arrayIORead, &
#endif
#ifdef _INTEGER_
                           arrayIsAlreadyIn, &
#endif
                           arrayIOWrite,  &
                           arrayArrayAbsMin, arrayArrayAbsMax, arrayArraySum

#ifdef _REAL_
   USE moduleMathematicArray1D, ONLY : mathArrayDot, setSecondWorkingArray, nullifySecondArrayPointer
#endif

   USE moduleValuesArray1DManagement, ONLY : memoryPutIn

! Procedures status
! =================

!  General part
!  ------------
#ifdef _REAL_
   PUBLIC :: arrayArrayDot
#endif

   PUBLIC :: arrayArrayPutIn

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

! Procedure 1 : make the dot product of 2 array 1D
! -------------------------------------------------
  FUNCTION arrayArrayDot(targetArray1,targetArray2) RESULT(val)

!     Declaration
!     - - - - - - -
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray1
      TYPE(arrayType), INTENT(IN) :: targetArray2
      CALL setWorkingArray(targetArray1)
      CALL setSecondWorkingArray(targetArray2)

!     Body
!     - - -
      val = mathArrayDot()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()
      CALL nullifySecondArrayPointer()

  END FUNCTION

#endif

! Procedure 2 : shift function
! ----------------------------
  SUBROUTINE arrayArrayPutIn(targetArray,iposition,value)

!    Declaration
!    - - - - - -
     INTEGER, INTENT(IN) :: iposition
     VARType, INTENT(IN) :: value

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryPutIn(iposition,value)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE


END MODULE moduleVector


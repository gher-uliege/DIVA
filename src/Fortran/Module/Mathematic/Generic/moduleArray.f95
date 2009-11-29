MODULE moduleArray

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
#define _ARRAY_1D_DEFINITION_
#define _ARRAY_2D_DEFINITION_
#define _ARRAY_3D_DEFINITION_

#ifdef _ARRAY_1D_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_
#endif

#ifdef _ARRAY_2D_
#undef _ARRAY_3D_DEFINITION_
#endif

! Include file
! ============
   USE moduleArrayDefinition

#ifdef _REAL_
   USE moduleNorm
#endif
   USE moduleInsertValueMethod

   USE moduleWorkingArray, ONLY : setWorkingArray, nullifyArrayPointer
   USE moduleValuesArrayManagement, ONLY : memoryGetValues, memoryGetValue, memoryGetPointerOnValue
   USE moduleValuesArrayNDManagement, ONLY : memoryArraySetToZero, memoryArraySetToValue, memoryArrayInsertValue, &
                                             memoryArrayAddValue, memoryArrayFastInsertValue, memoryArrayFastAddValue
   USE moduleMemoryArrayManagement, ONLY : memoryGetAllocationStatus, memorySetSize,  memorySetFirstIndex, &

#ifdef _ARRAY_1D_
                                           memoryGetFirstIndex, memoryGetLastIndex, memoryGetSize, memoryGetAllocatedSize , &
                                           memoryGetIncreaseSize, memoryGetDefaultIncreaseSize, &
#endif

#ifdef _ARRAY_1D_DEFINITION_
                                           memoryGetFirstIndexX, memoryGetLastIndexX, memoryGetSizeX, memoryGetAllocatedSizeX , &
                                           memoryGetIncreaseSizeX, memoryGetDefaultIncreaseSizeX , memorySetIncreaseSizeX, &
#endif

#ifdef _ARRAY_2D_DEFINITION_
                                           memoryGetFirstIndexY, memoryGetLastIndexY, memoryGetSizeY, memoryGetAllocatedSizeY , &
                                           memoryGetIncreaseSizeY, memoryGetDefaultIncreaseSizeY , memorySetIncreaseSizeY, &
#endif

#ifdef _ARRAY_3D_DEFINITION_
                                           memoryGetFirstIndexZ, memoryGetLastIndexZ, memoryGetSizeZ, memoryGetAllocatedSizeZ , &
                                           memoryGetIncreaseSizeZ, memoryGetDefaultIncreaseSizeZ, memorySetIncreaseSizeZ, &
#endif
                                           memorySetIncreaseSize, memoryDestructor

   USE moduleMemoryArrayNDManagement, ONLY : memoryPrintInformation, memoryArrayCreate, memoryAllocateArray, &
                                             memoryOptimize

   USE moduleMathematicArrayND, ONLY : mathArrayMin, mathArrayMax, &
#ifdef _REAL_
                                       mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
                                       mathArrayScale, &
#endif
                                       mathArrayAbsMin, mathArrayAbsMax
#ifdef _REAL_
   USE moduleIOArrayManagement, ONLY : ioArrayWrite
   USE moduleIOArrayManagementND, ONLY : ioArrayRead
   USE moduleFileDefinition
#endif

   INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: arrayGetValues, arrayGetValue, arrayGetAllocationStatus, arrayGetPointerOnValue
   PUBLIC :: arrayArraySetToZero, arrayArraySetToValue, arrayArrayInsertValue, arrayArrayAddValue, arrayArrayFastInsertValue, &
             arrayArrayFastAddValue
   PUBLIC :: arraySetIncreaseSize
   PUBLIC :: arrayDestructor, arrayPrintInformation, arrayCreateBase, arrayCreateWithDimension, &
             arrayCreateWithDimensionAndFirstIndex, arraySetSize, arrayOptimize

#ifdef _ARRAY_1D_
   PUBLIC :: arrayGetFirstIndex,arrayGetLastIndex, arrayGetSize, arrayGetAllocatedSize , arrayGetIncreaseSize
   PUBLIC :: arrayGetDefaultIncreaseSize
#endif

#ifdef _ARRAY_1D_DEFINITION_
   PUBLIC :: arrayGetFirstIndexX, arrayGetLastIndexX, arrayGetSizeX, arrayGetAllocatedSizeX , arrayGetIncreaseSizeX
   PUBLIC :: arrayGetDefaultIncreaseSizeX
   PUBLIC :: arraySetIncreaseSizeX
#endif

#ifdef _ARRAY_2D_DEFINITION_
   PUBLIC :: arrayGetFirstIndexY, arrayGetLastIndexY, arrayGetSizeY, arrayGetAllocatedSizeY , arrayGetIncreaseSizeY
   PUBLIC :: arrayGetDefaultIncreaseSizeY
   PUBLIC :: arraySetIncreaseSizeY
#endif

#ifdef _ARRAY_3D_DEFINITION_
   PUBLIC :: arrayGetFirstIndexZ, arrayGetLastIndexZ, arrayGetSizeZ, arrayGetAllocatedSizeZ , arrayGetIncreaseSizeZ
   PUBLIC :: arrayGetDefaultIncreaseSizeZ
   PUBLIC :: arraySetIncreaseSizeZ
#endif

   PUBLIC :: arrayArrayMin, arrayArrayMax, &
#ifdef _REAL_
             arrayArrayNorm1, arrayArrayNorm2, arrayArrayNormInfinity, arrayArrayNorm, arrayArraySqrt, arrayArraySum, &
             arrayArrayScale, &
#endif
             arrayArrayAbsMin, arrayArrayAbsMax

#ifdef _REAL_
   PUBLIC :: arrayIOWrite, arrayIORead
#endif

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
! ===            Internal procedure ("PUBLIC")  : Getting   ===
! =============================================================

! Procedure 1 : get reference to pointer containing the values
! ------------------------------------------------------------
   FUNCTION arrayGetValues(targetArray) RESULT(ptr)

!    Declaration
!    - - - - - - -

!        1) For 1D array
!        + + + + + + + + +
#ifdef _ARRAY_1D_
       VARType, DIMENSION(:), POINTER :: ptr
#endif

!        2) For 2D array
!        + + + + + + + +
#ifdef _ARRAY_2D_
       VARType, DIMENSION(:,:), POINTER :: ptr
#endif

!        3) For 3D array
!        + + + + + + + +
#ifdef _ARRAY_3D_
       VARType, DIMENSION(:,:,:), POINTER :: ptr
#endif

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ptr => memoryGetValues()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END FUNCTION

! Procedure 2 : get the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetValue(targetArray,i1) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = memoryGetValue(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  FUNCTION arrayGetValue(targetArray,i1,i2) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = memoryGetValue(i1,i2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  FUNCTION arrayGetValue(targetArray,i1,i2,i3) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = memoryGetValue(i1,i2,i3)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 3 : get the first index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetFirstIndex(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetFirstIndexX(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue =memoryGetFirstIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetFirstIndexY(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetFirstIndexZ(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 4 : get the last index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetLastIndex(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetLastIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetLastIndexX(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue =memoryGetLastIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetLastIndexY(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetLastIndexY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetLastIndexZ(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetLastIndexZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 5 : get the number of data of the array
! --------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetSize(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetSizeX(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetSizeY(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetSizeZ(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 6 : get the allocated dimension of the array
! -------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetAllocatedSize(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetAllocatedSizeX(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetAllocatedSizeY(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetAllocatedSizeZ(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 7 : get the increase dimension of the array
! -------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetIncreaseSize(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetIncreaseSizeX(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetIncreaseSizeY(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetIncreaseSizeZ(targetArray) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 8 : get the allocation status
! ----------------------------------------
  FUNCTION arrayGetAllocationStatus(targetArray) RESULT(status)

!     Declaration
!     - - - - - -
      LOGICAL :: status

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      status = memoryGetAllocationStatus()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION

! Procedure 9 : get a pointer on the value in the array at specified position
! ----------------------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetPointerOnValue(targetArray,i1) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType, POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  FUNCTION arrayGetPointerOnValue(targetArray,i1,i2) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
      VARType, POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1,i2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  FUNCTION arrayGetPointerOnValue(targetArray,i1,i2,i3) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2,i3
      VARType, POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1,i2,i3)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! Procedure 10 : get the default increase size of the array
! -------------------------------------------------------
  !    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION arrayGetDefaultIncreaseSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION arrayGetDefaultIncreaseSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION arrayGetDefaultIncreaseSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION arrayGetDefaultIncreaseSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END FUNCTION
#endif

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Setting   ===
! =============================================================

! Procedure 1 : set the increase dimension of the array
! ------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arraySetIncreaseSize(targetArray,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE arraySetIncreaseSizeX(targetArray,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arraySetIncreaseSize(targetArray,i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE arraySetIncreaseSizeY(targetArray,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeY(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arraySetIncreaseSize(targetArray,i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)
      CALL memorySetIncreaseSizeZ(i3)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE arraySetIncreaseSizeZ(targetArray,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetIncreaseSizeZ(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! Procedure 2 : insert the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arrayArrayInsertValue(targetArray,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayInsertValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arrayArrayInsertValue(targetArray,i1,i2,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayInsertValue(i1,i2,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arrayArrayInsertValue(targetArray,i1,i2,i3,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayInsertValue(i1,i2,i3,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! Procedure 3 : fast insert the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arrayArrayFastInsertValue(targetArray,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastInsertValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arrayArrayFastInsertValue(targetArray,i1,i2,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastInsertValue(i1,i2,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arrayArrayFastInsertValue(targetArray,i1,i2,i3,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastInsertValue(i1,i2,i3,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! Procedure 4 : insert the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arrayArrayAddValue(targetArray,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayAddValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arrayArrayAddValue(targetArray,i1,i2,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayAddValue(i1,i2,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arrayArrayAddValue(targetArray,i1,i2,i3,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayAddValue(i1,i2,i3,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! Procedure 5 : fast add the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arrayArrayFastAddValue(targetArray,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastAddValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arrayArrayFastAddValue(targetArray,i1,i2,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastAddValue(i1,i2,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arrayArrayFastAddValue(targetArray,i1,i2,i3,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayFastAddValue(i1,i2,i3,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! Procedure 6 : global procedure to insert the value in the array at specified position
! --------------------------------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arrayArraySetValue(targetArray,i1,val,modeType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(insertValueMethod), INTENT(IN) :: modeType
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      SELECT CASE (modeType%insertValueMode)
         CASE (INSERT_VALUE%insertValueMode)
             CALL memoryArrayInsertValue(i1,val)
         CASE (ADD_VALUE%insertValueMode)
             CALL memoryArrayAddValue(i1,val)
         CASE (FAST_INSERT_VALUE%insertValueMode)
             CALL memoryArrayFastInsertValue(i1,val)
         CASE (FAST_ADD_VALUE%insertValueMode)
             CALL memoryArrayFastAddValue(i1,val)
         CASE DEFAULT
             CALL memoryArrayInsertValue(i1,val)
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arrayArraySetValue(targetArray,i1,i2,val,modeType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      TYPE(insertValueMethod), INTENT(IN) :: modeType
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      SELECT CASE (modeType%insertValueMode)
         CASE (INSERT_VALUE%insertValueMode)
             CALL memoryArrayInsertValue(i1,i2,val)
         CASE (ADD_VALUE%insertValueMode)
             CALL memoryArrayAddValue(i1,i2,val)
         CASE (FAST_INSERT_VALUE%insertValueMode)
             CALL memoryArrayFastInsertValue(i1,i2,val)
         CASE (FAST_ADD_VALUE%insertValueMode)
             CALL memoryArrayFastAddValue(i1,i2,val)
         CASE DEFAULT
             CALL memoryArrayInsertValue(i1,i2,val)
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arrayArraySetValue(targetArray,i1,i2,i3,val,modeType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      TYPE(insertValueMethod), INTENT(IN) :: modeType
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      SELECT CASE (modeType%insertValueMode)
         CASE (INSERT_VALUE%insertValueMode)
             CALL memoryArrayInsertValue(i1,i2,i3,val)
         CASE (ADD_VALUE%insertValueMode)
             CALL memoryArrayAddValue(i1,i2,i3,val)
         CASE (FAST_INSERT_VALUE%insertValueMode)
             CALL memoryArrayFastInsertValue(i1,i2,i3,val)
         CASE (FAST_ADD_VALUE%insertValueMode)
             CALL memoryArrayFastAddValue(i1,i2,i3,val)
         CASE DEFAULT
             CALL memoryArrayInsertValue(i1,i2,i3,val)
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1 : deallocation of the memory
! ------------------------------------------
  SUBROUTINE arrayDestructor(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryDestructor()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 2 : set array to zero
! -------------------------------
  SUBROUTINE arrayArraySetToZero(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArraySetToZero()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 2 : set array to zero
! -------------------------------
  SUBROUTINE arrayArraySetToValue(targetArray,val)

!     Declaration
!     - - - - - -
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArraySetToValue(val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 3 : print information on the array
! ---------------------------------------------
   SUBROUTINE arrayPrintInformation(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryPrintInformation()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

! Procedure 4 : create the array (only array pointer)
! -------------------------------
   SUBROUTINE arrayCreateBase(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

! Procedure 5 : create the array (with dimension)
! -------------------------------

!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
   SUBROUTINE arrayCreateWithDimension(targetArray, sizeX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetSize(sizeX)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
   SUBROUTINE arrayCreateWithDimension(targetArray, sizeX, sizeY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, sizeY

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetSize(sizeX,sizeY)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
   SUBROUTINE arrayCreateWithDimension(targetArray, sizeX, sizeY, sizeZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, sizeY, sizeZ

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetSize(sizeX,sizeY,sizeZ)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif

! Procedure 6 : create the array (with dimension and istartingValue)
! -------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
   SUBROUTINE arrayCreateWithDimensionAndFirstIndex(targetArray, sizeX, istartingValueX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, istartingValueX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetFirstIndex(istartingValueX)
      CALL memorySetSize(sizeX)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
   SUBROUTINE arrayCreateWithDimensionAndFirstIndex(targetArray, sizeX, sizeY, istartingValueX, istartingValueY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, sizeY, istartingValueX, istartingValueY

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetFirstIndex(istartingValueX,istartingValueY)
      CALL memorySetSize(sizeX,sizeY)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
   SUBROUTINE arrayCreateWithDimensionAndFirstIndex(targetArray, sizeX, sizeY, sizeZ, istartingValueX, istartingValueY, &
                                                       istartingValueZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, sizeY, sizeZ, istartingValueX, istartingValueY, istartingValueZ

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryArrayCreate()
      CALL memorySetFirstIndex(istartingValueX,istartingValueY,istartingValueZ)
      CALL memorySetSize(sizeX,sizeY,sizeZ)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE
#endif


! Procedure 7 : define the size of the array
! -------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE arraySetSize(targetArray,dimX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: dimX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetSize(dimX)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE arraySetSize(targetArray,dimX,dimY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: dimX,dimY

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetSize(dimX,dimY)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE arraySetSize(targetArray,dimX,dimY,dimZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: dimX,dimY,dimZ

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memorySetSize(dimX,dimY,dimZ)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

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
         CASE (NORM_L1%normTypeValue)
            val = mathArrayNorm1()
         CASE (NORM_L2%normTypeValue)
            val = mathArrayNorm2()
         CASE (NORM_INF%normTypeValue)
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

! Procedure 12 : optimization of the memory
! -----------------------------------------
  SUBROUTINE arrayOptimize(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryOptimize()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

#ifdef _REAL_
! Procedure 13 : writing procedure
! --------------------------------
  SUBROUTINE arrayIOWrite(targetArray,fileToWrite,exclusionValue)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      REAL(KIND=4), OPTIONAL, INTENT(IN) :: exclusionValue
      REAL(KIND=4) :: realExclusionValue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      realExclusionValue = posInf

      IF ( PRESENT(exclusionValue) ) THEN
         realExclusionValue = exclusionValue
      ENDIF

      CALL ioArrayWrite(fileToWrite,realExclusionValue)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE

! Procedure 13 : writing procedure
! --------------------------------
  SUBROUTINE arrayIORead(targetArray,fileToRead,exclusionValue)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      REAL(KIND=4), OPTIONAL, INTENT(OUT) :: exclusionValue
      REAL(KIND=4) :: realExclusionValue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL ioArrayRead(fileToRead,realExclusionValue)

      IF ( PRESENT(exclusionValue) ) THEN
         exclusionValue = realExclusionValue
      ENDIF


!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

  END SUBROUTINE
#endif

#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

END MODULE moduleArray


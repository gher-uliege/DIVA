MODULE moduleGenericTypeDataBase

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
#define _DB_1D_DEFINITION_
#define _DB_2D_DEFINITION_
#define _DB_3D_DEFINITION_

#ifdef _DB_1D_
#undef _DB_2D_DEFINITION_
#undef _DB_3D_DEFINITION_
#endif

#ifdef _DB_2D_
#undef _DB_3D_DEFINITION_
#endif

! Include file
! ============
   USE moduleGenericTypeDataBaseDefinition

   USE moduleWorkingDataBase, ONLY : setWorkingDataBase, nullifyDataBasePointer
   USE moduleValuesDataBaseManagement, ONLY : memoryGetValues,memoryGetPointerOnValue
   USE moduleMemoryDataBaseManagement, ONLY : memoryGetAllocationStatus, memorySetFirstIndex, &

#ifdef _DB_1D_
                                           memoryGetFirstIndex, memoryGetLastIndex, memoryGetSize, memoryGetAllocatedSize , &
                                           memoryGetIncreaseSize, memoryGetDefaultIncreaseSize, &
#endif

#ifdef _DB_1D_DEFINITION_
                                           memoryGetFirstIndexX, memoryGetLastIndexX, memoryGetSizeX, memoryGetAllocatedSizeX , &
                                           memoryGetIncreaseSizeX, memoryGetDefaultIncreaseSizeX , memorySetIncreaseSizeX, &
#endif

#ifdef _DB_2D_DEFINITION_
                                           memoryGetFirstIndexY, memoryGetLastIndexY, memoryGetSizeY, memoryGetAllocatedSizeY , &
                                           memoryGetIncreaseSizeY, memoryGetDefaultIncreaseSizeY , memorySetIncreaseSizeY, &
#endif

#ifdef _DB_3D_DEFINITION_
                                           memoryGetFirstIndexZ, memoryGetLastIndexZ, memoryGetSizeZ, memoryGetAllocatedSizeZ , &
                                           memoryGetIncreaseSizeZ, memoryGetDefaultIncreaseSizeZ, memorySetIncreaseSizeZ, &
#endif
                                           memorySetIncreaseSize, memoryDestructor

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: dataBaseGetValues, dataBaseGetAllocationStatus, dataBaseGetPointerOnValue
   PUBLIC :: dataBaseSetIncreaseSize
   PUBLIC :: dataBaseDestructor

#ifdef _DB_1D_
   PUBLIC :: dataBaseGetFirstIndex,dataBaseGetLastIndex, dataBaseGetSize, dataBaseGetAllocatedSize , dataBaseGetIncreaseSize
   PUBLIC :: dataBaseGetDefaultIncreaseSize
#endif

#ifdef _DB_1D_DEFINITION_
   PUBLIC :: dataBaseGetFirstIndexX, dataBaseGetLastIndexX, dataBaseGetSizeX, dataBaseGetAllocatedSizeX , dataBaseGetIncreaseSizeX
   PUBLIC :: dataBaseGetDefaultIncreaseSizeX
   PUBLIC :: dataBaseSetIncreaseSizeX
#endif

#ifdef _DB_2D_DEFINITION_
   PUBLIC :: dataBaseGetFirstIndexY, dataBaseGetLastIndexY, dataBaseGetSizeY, dataBaseGetAllocatedSizeY , dataBaseGetIncreaseSizeY
   PUBLIC :: dataBaseGetDefaultIncreaseSizeY
   PUBLIC :: dataBaseSetIncreaseSizeY
#endif

#ifdef _DB_3D_DEFINITION_
   PUBLIC :: dataBaseGetFirstIndexZ, dataBaseGetLastIndexZ, dataBaseGetSizeZ, dataBaseGetAllocatedSizeZ , dataBaseGetIncreaseSizeZ
   PUBLIC :: dataBaseGetDefaultIncreaseSizeZ
   PUBLIC :: dataBaseSetIncreaseSizeZ
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
   FUNCTION dataBaseGetValues(targetDataBase) RESULT(ptr)

!    Declaration
!    - - - - - - -

!        1) For 1D dataBase
!        + + + + + + + + +
#ifdef _DB_1D_
       TYPE(genericType), DIMENSION(:), POINTER :: ptr
#endif

!        2) For 2D dataBase
!        + + + + + + + +
#ifdef _DB_2D_
       TYPE(genericType), DIMENSION(:,:), POINTER :: ptr
#endif

!        3) For 3D dataBase
!        + + + + + + + +
#ifdef _DB_3D_
       TYPE(genericType), DIMENSION(:,:,:), POINTER :: ptr
#endif

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ptr => memoryGetValues()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END FUNCTION

! Procedure 2 : get the first index of the dataBase
! ----------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetFirstIndex(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetFirstIndexX(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue =memoryGetFirstIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetFirstIndexY(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetFirstIndexZ(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetFirstIndexZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 3 : get the last index of the dataBase
! ----------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetLastIndex(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetLastIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetLastIndexX(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue =memoryGetLastIndexX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetLastIndexY(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetLastIndexY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetLastIndexZ(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetLastIndexZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 4 : get the number of data of the dataBase
! --------------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetSize(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetSizeX(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetSizeY(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetSizeZ(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 5 : get the allocated dimension of the dataBase
! -------------------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetAllocatedSize(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetAllocatedSizeX(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetAllocatedSizeY(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetAllocatedSizeZ(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 6 : get the increase dimension of the dataBase
! -------------------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetIncreaseSize(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetIncreaseSizeX(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetIncreaseSizeY(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetIncreaseSizeZ(targetDataBase) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 7 : get the allocation status
! ----------------------------------------
  FUNCTION dataBaseGetAllocationStatus(targetDataBase) RESULT(status)

!     Declaration
!     - - - - - -
      LOGICAL :: status

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      status = memoryGetAllocationStatus()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION

! Procedure 8 : get a pointer on the value in the dataBase at specified position
! ----------------------------------------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetPointerOnValue(targetDataBase,i1) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(genericType), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_
  FUNCTION dataBaseGetPointerOnValue(targetDataBase,i1,i2) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
      TYPE(genericType), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1,i2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_
  FUNCTION dataBaseGetPointerOnValue(targetDataBase,i1,i2,i3) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2,i3
      TYPE(genericType), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(i1,i2,i3)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! Procedure 9 : get the default increase size of the dataBase
! -------------------------------------------------------
  !    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION dataBaseGetDefaultIncreaseSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

#ifdef _DB_1D_DEFINITION_
  FUNCTION dataBaseGetDefaultIncreaseSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_DEFINITION_
  FUNCTION dataBaseGetDefaultIncreaseSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_DEFINITION_
  FUNCTION dataBaseGetDefaultIncreaseSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION
#endif

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Setting   ===
! =============================================================

! Procedure 1 : set the increase dimension of the dataBase
! ------------------------------------------------------
!    Procedure A : for 1D dataBase
!    ----------------------------
#ifdef _DB_1D_
  SUBROUTINE dataBaseSetIncreaseSize(targetDataBase,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

#ifdef _DB_1D_DEFINITION_
  SUBROUTINE dataBaseSetIncreaseSizeX(targetDataBase,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

!    Procedure B : for 2D dataBase
!    ----------------------------
#ifdef _DB_2D_
  SUBROUTINE dataBaseSetIncreaseSize(targetDataBase,i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

#ifdef _DB_2D_DEFINITION_
  SUBROUTINE dataBaseSetIncreaseSizeY(targetDataBase,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeY(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

!    Procedure C : for 3D dataBase
!    ----------------------------
#ifdef _DB_3D_
  SUBROUTINE dataBaseSetIncreaseSize(targetDataBase,i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)
      CALL memorySetIncreaseSizeZ(i3)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

#ifdef _DB_3D_DEFINITION_
  SUBROUTINE dataBaseSetIncreaseSizeZ(targetDataBase,i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetIncreaseSizeZ(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE
#endif

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1 : deallocation of the memory
! ------------------------------------------
  SUBROUTINE dataBaseDestructor(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDestructor()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE


#undef _DB_1D_DEFINITION_
#undef _DB_2D_DEFINITION_
#undef _DB_3D_DEFINITION_

END MODULE moduleGenericTypeDataBase


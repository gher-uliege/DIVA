MODULE moduleGenericTypeDataBase1D

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
   USE moduleGenericTypeDataBaseDefinition

   USE moduleWorkingDataBase, ONLY : setWorkingDataBase, nullifyDataBasePointer
   USE moduleGenericTypeDataBase, ONLY : dataBaseGetValues, dataBaseGetAllocationStatus, dataBaseGetPointerOnValue, &
                           dataBaseSetIncreaseSize, dataBaseDestructor, &
                           dataBaseGetFirstIndex,dataBaseGetLastIndex, dataBaseGetSize, dataBaseGetAllocatedSize , &
                           dataBaseGetIncreaseSize, &
                           dataBaseGetDefaultIncreaseSize, &
                           dataBaseGetFirstIndexX, dataBaseGetLastIndexX, dataBaseGetSizeX, dataBaseGetAllocatedSizeX , &
                           dataBaseGetIncreaseSizeX, dataBaseGetDefaultIncreaseSizeX, dataBaseSetIncreaseSizeX
   USE moduleMemoryDataBaseManagement, ONLY : memorySetSize, memorySetFirstIndex
   USE moduleMemoryDataBase1DManagement, ONLY : memoryDataBaseCreate, memoryAllocateDataBase, memoryPrintInformation
   USE moduleValuesDataBase1DManagement, ONLY : memoryDataBaseInsertValue,memoryDataBaseFastInsertValue

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: dataBaseCreateBase, dataBaseCreateWithDimension, dataBaseCreateWithDimensionAndFirstIndex, &
             dataBaseSetSize, dataBasePrintInformation, dataBaseInsertElement, dataBaseFastInsertElement


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
! Procedure 1 : create the array (only array pointer)
! -------------------------------
   SUBROUTINE dataBaseCreateBase(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDataBaseCreate()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 2 : create the array (with dimension)
! -------------------------------
   SUBROUTINE dataBaseCreateWithDimension(targetDataBase, sizeX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDataBaseCreate()
      CALL memorySetSize(sizeX)
      CALL memoryAllocateDataBase()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 3 : create the array (with dimension and istartingValue)
! -------------------------------
   SUBROUTINE dataBaseCreateWithDimensionAndFirstIndex(targetDataBase, sizeX, istartingValueX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, istartingValueX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDataBaseCreate()
      CALL memorySetFirstIndex(istartingValueX)
      CALL memorySetSize(sizeX)
      CALL memoryAllocateDataBase()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 4 : define the size of the array
! -------------------------------------------
  SUBROUTINE dataBaseSetSize(targetDataBase,dimX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: dimX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memorySetSize(dimX)
      CALL memoryAllocateDataBase()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 5 : print information on the array
! ---------------------------------------------
   SUBROUTINE dataBasePrintInformation(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryPrintInformation()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 6 : insert an element in the database
! ------------------------------------------------
  SUBROUTINE dataBaseInsertElement(targetDataBase,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(genericType), TARGET, INTENT(IN) :: val
      TYPE(genericType), POINTER :: val2

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      val2 => val
      CALL memoryDataBaseInsertValue(i1,val2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 7 : fast insert an element in the database
! ----------------------------------------------------
  SUBROUTINE dataBaseFastInsertElement(targetDataBase,i1,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(genericType), TARGET, INTENT(IN) :: val
      TYPE(genericType), POINTER :: val2

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      val2 => val
      CALL memoryDataBaseFastInsertValue(i1,val2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

END MODULE moduleGenericTypeDataBase1D


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

   USE moduleInsertValueMethod
   USE moduleWorkingDataBase, ONLY : setWorkingDataBase, nullifyDataBasePointer
   USE moduleGenericTypeDataBase, ONLY : dataBaseGetValues, dataBaseGetAllocationStatus, dataBaseGetPointerOnValue, &
                           dataBaseSetIncreaseSize, dataBaseDestructor, &
                           dataBaseGetFirstIndex,dataBaseGetLastIndex, dataBaseGetSize, dataBaseGetAllocatedSize , &
                           dataBaseGetIncreaseSize, &
                           dataBaseGetDefaultIncreaseSize, &
                           dataBaseGetFirstIndexX, dataBaseGetLastIndexX, dataBaseGetSizeX, dataBaseGetAllocatedSizeX , &
                           dataBaseGetIncreaseSizeX, dataBaseGetDefaultIncreaseSizeX, dataBaseSetIncreaseSizeX
   USE moduleMemoryDataBaseManagement, ONLY : memorySetSize, memorySetFirstIndex, memoryGetSize, memoryDefineLastIndex, &
                                              memoryGetLastIndex
   USE moduleMemoryDataBase1DManagement, ONLY : memoryDataBaseCreate, memoryAllocateDataBase, memoryPrintInformation, &
                                                memoryOptimize
   USE moduleValuesDataBase1DManagement, ONLY : memoryDataBaseInsertValue,memoryDataBaseFastInsertValue, &
                                                memoryDataBaseInitialise, memoryDataBaseSetValue, &
                                                memoryDataBaseDestroy
   USE moduleValuesDataBaseManagement, ONLY : memoryGetPointerOnValue
! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: dataBaseCreateBase, dataBaseCreateWithDimension, dataBaseCreateWithDimensionAndFirstIndex, &
             dataBaseSetSize, dataBasePrintInformation, dataBaseInsertElement, dataBaseFastInsertElement, &
             dataBaseInitialise, dataBaseSetValue, dataBaseOptimizeMemory,dataBaseAddSize, &
             dataBasePushBackElement, dataBaseFastPushBackElement, dataBaseGetPointerOnLastValue, &
             dataBaseDestroy


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
      CALL memoryDataBaseInitialise()

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
      CALL memoryDataBaseInitialise()

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
      CALL memoryDataBaseInitialise()

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

! Procedure 8 : initialisation
! ----------------------------
   SUBROUTINE dataBaseInitialise(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDataBaseInitialise()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 9 : global procedure to insert the value in the array at specified position
! --------------------------------------------------------------------------------------
  SUBROUTINE dataBaseSetValue(targetDataBase,i1,val,modeType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(insertValueMethod), INTENT(IN) :: modeType
      TYPE(genericType), TARGET, INTENT(IN) :: val
      TYPE(genericType), POINTER :: val2

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      val2 => val
      CALL memoryDataBaseSetValue(i1,val2,modeType)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 10 : optimisation of the memory
! ----------------------------------------
   SUBROUTINE dataBaseOptimizeMemory(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryOptimize()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

! Procedure 11 : define the size of the array by increasing it of "value"
! -----------------------------------------------------------------------
  SUBROUTINE dataBaseAddSize(targetDataBase,addDimX)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: addDimX
      INTEGER :: oldSize

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      oldSize = memoryGetSize()
      CALL memorySetSize(oldSize+addDimX)
      CALL memoryAllocateDataBase()
      CALL memorySetSize(oldSize)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 12 : put an element at the end of the database
! --------------------------------------------------------
  SUBROUTINE dataBasePushBackElement(targetDataBase,val)

!     Declaration
!     - - - - - -
      TYPE(genericType), TARGET, INTENT(IN) :: val
      TYPE(genericType), POINTER :: val2

      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      i1 = memoryGetSize() + 1
      val2 => val
      CALL memoryDataBaseInsertValue(i1,val2)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 13 : fast put an element at the end of the database
! -------------------------------------------------------------
  SUBROUTINE dataBaseFastPushBackElement(targetDataBase,val)

!     Declaration
!     - - - - - -
      TYPE(genericType), TARGET, INTENT(IN) :: val
      TYPE(genericType), POINTER :: val2

      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      i1 = memoryGetSize() + 1
      val2 => val
      CALL memoryDataBaseFastInsertValue(i1,val2)
      CALL memorySetSize(i1)
      CALL memoryDefineLastIndex()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END SUBROUTINE

! Procedure 14 : get the last element
! -----------------------------------
  FUNCTION dataBaseGetPointerOnLastValue(targetDataBase) RESULT(ptr)

!     Declaration
!     - - - - - -
      TYPE(genericType), POINTER :: ptr
      INTEGER :: iEndX

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      iEndX = memoryGetLastIndex()
      ptr => memoryGetPointerOnValue(iEndX)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

  END FUNCTION

! Procedure 14 : destructor
! ----------------------------
   SUBROUTINE dataBaseDestroy(targetDataBase)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN) :: targetDataBase
      CALL setWorkingDataBase(targetDataBase)

!     Body
!     - - -
      CALL memoryDataBaseDestroy()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyDataBasePointer()

   END SUBROUTINE

END MODULE moduleGenericTypeDataBase1D


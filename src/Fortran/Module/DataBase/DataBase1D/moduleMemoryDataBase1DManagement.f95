MODULE moduleMemoryDataBase1DManagement

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
   USE moduleGenericTypeDefinition
   USE moduleWorkingDataBase, ONLY : workingDataBase
   USE moduleMemoryDataBaseManagement, ONLY : memoryDataBaseDefine, memoryGetFirstIndexX, memoryGetLastIndexX, &
                                           memoryGetSizeX, &
                                           memoryGetAllocatedSizeX, &
                                           memoryDestructor, memorySetFirstIndex, memorySetAllocatedSize, &
                                           memoryGetIncreaseSizeX, &
                                           memorySetSize, memoryAllocateMemory, memoryFirstAllocateMemory, &
                                           memoryGetAllocationStatus, memoryDefineLastIndex
   USE moduleValuesDataBaseManagement, ONLY : memoryGetPointerOnValue
   USE moduleGenericTypeSurDefined, ONLY : genericTypePrintInformation => printInformation

   INCLUDE 'ioParameter.h'

! Declaration
! ===========

!  General part
!  ------------
   TYPE(genericType), DIMENSION(:), PRIVATE, POINTER :: internalWorkingDataBase

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryDataBaseCreate, memoryAllocateDataBase, memoryPrintInformation, memoryOptimize
   PRIVATE :: memoryStockIntermediateDataBase, memoryTransferIntermediateDBToDB


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
! Procedure 1 : create the array
! ---------------------------------
   SUBROUTINE memoryDataBaseCreate()

!     Body
!     - - -
      CALL memoryDataBaseDefine()
      CALL memoryAllocateDataBase()

   END SUBROUTINE

! Procedure 2 : allocated memory to the array
! --------------------------------------------
   SUBROUTINE memoryAllocateDataBase()

!     Declaration
!     - - - - - -
      INTEGER :: newSizeX, istartValueX, istartX
      INTEGER, DIMENSION(1) :: istartTab

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartTab = lbound(workingDataBase%values)
            istartX = istartTab(1)
            istartValueX = memoryGetFirstIndexX()

            IF ((memoryGetSizeX() > memoryGetAllocatedSizeX()).OR.(istartValueX<istartX)) THEN
                newSizeX = memoryGetSizeX()
                CALL memoryStockIntermediateDataBase()
                CALL memoryDestructor()
                CALL memorySetFirstIndex(istartValueX)
                CALL memorySetAllocatedSize(newSizeX+memoryGetIncreaseSizeX())
                CALL memorySetSize(newSizeX)
                CALL memoryDefineLastIndex()
                CALL memoryAllocateMemory()
                CALL memoryTransferIntermediateDBToDB()
            END IF
         CASE (.FALSE.)
            CALL memoryFirstAllocateMemory()
      END SELECT

   END SUBROUTINE

! Procedure 3 : transfer data from workingArray to secondworkingArray
! ----------------------------------------------------------------------
  SUBROUTINE memoryStockIntermediateDataBase()

!     Declaration
!     - - - - - -
      INTEGER :: istartX,iendX
      INTEGER, DIMENSION(1) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(workingDataBase%values)
      iendTab = ubound(workingDataBase%values)
      istartX = istartTab(1)
      iendX = iendTab(1)

      ALLOCATE(internalWorkingDataBase(istartX:iendX))

      internalWorkingDataBase(istartX:iendX) = workingDataBase%values(istartX:iendX)

  END SUBROUTINE

! Procedure 4 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateDBToDB()

!     Declaration
!     - - - - - -
      INTEGER :: istartX,iendX
      INTEGER, DIMENSION(1) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingDataBase)
      iendTab = ubound(internalWorkingDataBase)
      istartX = max(istartTab(1),memoryGetFirstIndexX())
      iendX = min(iendTab(1),memoryGetLastIndexX())

      workingDataBase%values(istartX:iendX) = internalWorkingDataBase(istartX:iendX)

      DEALLOCATE(internalWorkingDataBase)

  END SUBROUTINE

! Procedure 5 : print information
! -------------------------------
   SUBROUTINE memoryPrintInformation()

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX,iendX

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()


      WRITE(stdOutput,*) 'The size of the data base is : ', memoryGetSizeX()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX()
      WRITE(stdOutput,*) '   The increase allocation memory is : ', memoryGetIncreaseSizeX()
      WRITE(stdOutput,*) '   Allocation status of the data base : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetFirstIndexX()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetLastIndexX()

      IF (memoryGetAllocationStatus()) THEN
         DO i1 = istartX, iendX
            WRITE(stdOutput,*) 'value(',i1,') is '
            CALL genericTypePrintInformation(stdOutput,memoryGetPointerOnValue(i1))
         ENDDO
      END IF

   END SUBROUTINE

! Procedure 6 : optimisation of the allocated memory
! --------------------------------------------------
   SUBROUTINE memoryOptimize()

!     Declaration
!     - - - - - -
      INTEGER :: newSizeX, istartValueX

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartValueX = memoryGetFirstIndexX()
            newSizeX = memoryGetSizeX()
            CALL memoryStockIntermediateDataBase()
            CALL memoryDestructor()
            CALL memorySetFirstIndex(istartValueX)
            CALL memorySetAllocatedSize(newSizeX)
            CALL memorySetSize(newSizeX)
            CALL memoryDefineLastIndex()
            CALL memoryAllocateMemory()
            CALL memoryTransferIntermediateDBToDB()
      END SELECT

   END SUBROUTINE


END MODULE moduleMemoryDataBase1DManagement

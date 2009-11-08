MODULE moduleMemoryArray1DManagement

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
   USE moduleWorkingArray, ONLY : workingArray
   USE moduleMemoryArrayManagement, ONLY : memoryArrayDefine, memoryGetFirstIndexX, memoryGetLastIndexX, &
                                           memoryGetSizeX, &
                                           memoryGetAllocatedSizeX, &
                                           memoryDestructor, memorySetFirstIndex, memorySetAllocatedSize, &
                                           memoryGetIncreaseSizeX, &
                                           memorySetSize, memoryAllocateMemory, memoryFirstAllocateMemory, &
                                           memoryGetAllocationStatus, memoryDefineLastIndex
   USE moduleValuesArrayManagement, ONLY : memoryGetValue

   INCLUDE 'ioParameter.h'

! Declaration
! ===========

!  General part
!  ------------
   VARType, DIMENSION(:), PRIVATE, POINTER :: internalWorkingValues

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryArrayCreate, memoryAllocateArray, memoryPrintInformation
   PRIVATE :: memoryStockIntermediateArray, memoryTransferIntermediateArrayToArray


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
   SUBROUTINE memoryArrayCreate()

!     Body
!     - - -
      CALL memoryArrayDefine()
      CALL memoryAllocateArray()

   END SUBROUTINE

! Procedure 2 : allocated memory to the array
! --------------------------------------------
   SUBROUTINE memoryAllocateArray()

!     Declaration
!     - - - - - -
      INTEGER :: newSizeX, istartValueX, istartX
      INTEGER, DIMENSION(1) :: istartTab

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartTab = lbound(workingArray%values)
            istartX = istartTab(1)
            istartValueX = memoryGetFirstIndexX()

            IF ((memoryGetSizeX() >= memoryGetAllocatedSizeX()).OR.(istartValueX<istartX)) THEN
                newSizeX = memoryGetSizeX()
                CALL memoryStockIntermediateArray()
                CALL memoryDestructor()
                CALL memorySetFirstIndex(istartValueX)
                CALL memorySetAllocatedSize(newSizeX+memoryGetIncreaseSizeX())
                CALL memorySetSize(newSizeX)
                CALL memoryDefineLastIndex()
                CALL memoryAllocateMemory()
                CALL memoryTransferIntermediateArrayToArray()
            END IF
         CASE (.FALSE.)
            CALL memoryFirstAllocateMemory()
      END SELECT

   END SUBROUTINE

! Procedure 3 : transfer data from workingArray to secondworkingArray
! ----------------------------------------------------------------------
  SUBROUTINE memoryStockIntermediateArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX,iendX
      INTEGER, DIMENSION(1) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(workingArray%values)
      iendTab = ubound(workingArray%values)
      istartX = istartTab(1)
      iendX = iendTab(1)

      ALLOCATE(internalWorkingValues(istartX:iendX))

      DO i1 = istartX , iendX
         internalWorkingValues(i1) = workingArray%values(i1)
      END DO

  END SUBROUTINE

! Procedure 4 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateArrayToArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, istartX,iendX
      INTEGER, DIMENSION(1) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingValues)
      iendTab = ubound(internalWorkingValues)
      istartX = istartTab(1)
      iendX = iendTab(1)

      DO i1 = istartX , iendX
         workingArray%values(i1) = internalWorkingValues(i1)
      END DO

      DEALLOCATE(internalWorkingValues)

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


      WRITE(stdOutput,*) 'The size of the array is : ', memoryGetSizeX()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX()
      WRITE(stdOutput,*) '   The increase allocation memory is : ', memoryGetIncreaseSizeX()
      WRITE(stdOutput,*) '   Allocation status of the vector : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetFirstIndexX()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetLastIndexX()

      IF (memoryGetAllocationStatus()) THEN
         DO i1 = istartX, iendX
            WRITE(stdOutput,*) 'value(',i1,') = ', memoryGetValue(i1)
         ENDDO
      END IF

   END SUBROUTINE

END MODULE moduleMemoryArray1DManagement

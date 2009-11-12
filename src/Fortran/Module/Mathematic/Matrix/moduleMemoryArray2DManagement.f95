MODULE moduleMemoryArray2DManagement

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
   USE moduleMemoryArrayManagement, ONLY : memoryArrayDefine, memoryGetFirstIndexX, memoryGetFirstIndexY, &
                                           memoryGetLastIndexX, memoryGetLastIndexY, &
                                           memoryGetSizeX, memoryGetSizeY, &
                                           memoryGetAllocatedSizeX, memoryGetAllocatedSizeY, &
                                           memoryDestructor, memorySetFirstIndex, memorySetAllocatedSize, &
                                           memoryGetIncreaseSizeX, memoryGetIncreaseSizeY, &
                                           memorySetSize, memoryAllocateMemory, memoryFirstAllocateMemory, &
                                           memoryGetAllocationStatus, memoryDefineLastIndex
   USE moduleValuesArrayManagement, ONLY : memoryGetValue

   INCLUDE 'ioParameter.h'

! Declaration
! ===========

!  General part
!  ------------
   VARType, DIMENSION(:,:), PRIVATE, POINTER :: internalWorkingValues

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryArrayCreate, memoryAllocateArray, memoryPrintInformation, memoryOptimize
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
      INTEGER :: newSizeX, newSizeY, istartValueX, istartValueY, istartX, istartY
      INTEGER, DIMENSION(2) :: istartTab

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartTab = lbound(workingArray%values)
            istartX = istartTab(1)
            istartY = istartTab(2)
            istartValueX = memoryGetFirstIndexX()
            istartValueY = memoryGetFirstIndexY()

            IF ((memoryGetSizeX() > memoryGetAllocatedSizeX()).OR.(istartValueX<istartX) &
                .OR. &
                (memoryGetSizeY() > memoryGetAllocatedSizeY()).OR.(istartValueY<istartY)) THEN
                newSizeX = memoryGetSizeX()
                newSizeY = memoryGetSizeY()
                CALL memoryStockIntermediateArray()
                CALL memoryDestructor()
                CALL memorySetFirstIndex(istartValueX,istartValueY)
                CALL memorySetAllocatedSize(newSizeX+memoryGetIncreaseSizeX(),newSizeY+memoryGetIncreaseSizeY())
                CALL memorySetSize(newSizeX,newSizeY)
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
      INTEGER :: i1, i2, istartX,iendX, istartY,iendY
      INTEGER, DIMENSION(2) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(workingArray%values)
      iendTab = ubound(workingArray%values)
      istartX = istartTab(1)
      istartY = istartTab(2)
      iendX = iendTab(1)
      iendY = iendTab(2)

      ALLOCATE(internalWorkingValues(istartX:iendX,istartY:iendY))

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         internalWorkingValues(i1,i2) = workingArray%values(i1,i2)
       END DO
      END DO

  END SUBROUTINE

! Procedure 4 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateArrayToArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX,iendX, istartY,iendY
      INTEGER, DIMENSION(2) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingValues)
      iendTab = ubound(internalWorkingValues)
      istartX = max(istartTab(1),memoryGetFirstIndexX())
      istartY = max(istartTab(2),memoryGetFirstIndexY())
      iendX = min(iendTab(1),memoryGetLastIndexX())
      iendY = min(iendTab(2),memoryGetLastIndexY())

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         workingArray%values(i1,i2) = internalWorkingValues(i1,i2)
       END DO
      END DO

      DEALLOCATE(internalWorkingValues)

  END SUBROUTINE

! Procedure 5 : print information
! -------------------------------
   SUBROUTINE memoryPrintInformation()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX,iendX, istartY,iendY

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()


      WRITE(stdOutput,*) 'The size of the array is : ', memoryGetSizeX(), ' ', memoryGetSizeY()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX(), ' ',  memoryGetAllocatedSizeY()
      WRITE(stdOutput,*) '   The increase allocation memory is : ', memoryGetIncreaseSizeX(),' ', memoryGetIncreaseSizeY()
      WRITE(stdOutput,*) '   Allocation status of the vector : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetFirstIndexX(),' ', memoryGetFirstIndexY()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetLastIndexX() ,' ',&
                                                      memoryGetLastIndexY()

      IF (memoryGetAllocationStatus()) THEN
         DO i1 = istartX, iendX
          DO i2 = istartY, iendY
            WRITE(stdOutput,*) 'value(',i1,',',i2,') = ', memoryGetValue(i1,i2)
          ENDDO
         ENDDO
      END IF

   END SUBROUTINE

! Procedure 6 : optimisation of the allocated memory
! --------------------------------------------------
   SUBROUTINE memoryOptimize()

!     Declaration
!     - - - - - -
      INTEGER :: newSizeX, newSizeY, istartValueX, istartValueY

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartValueX = memoryGetFirstIndexX()
            istartValueY = memoryGetFirstIndexY()

                newSizeX = memoryGetSizeX()
                newSizeY = memoryGetSizeY()
                CALL memoryStockIntermediateArray()
                CALL memoryDestructor()
                CALL memorySetFirstIndex(istartValueX,istartValueY)
                CALL memorySetAllocatedSize(newSizeX,newSizeY)
                CALL memorySetSize(newSizeX,newSizeY)
                CALL memoryDefineLastIndex()
                CALL memoryAllocateMemory()
                CALL memoryTransferIntermediateArrayToArray()
      END SELECT

   END SUBROUTINE

END MODULE moduleMemoryArray2DManagement

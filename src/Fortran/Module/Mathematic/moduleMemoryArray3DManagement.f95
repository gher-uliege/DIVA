MODULE moduleMemoryArray3DManagement

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
                                           memoryGetFirstIndexZ, &
                                           memoryGetLastIndexX, memoryGetLastIndexY, memoryGetLastIndexZ, &
                                           memoryGetSizeX, memoryGetSizeY, memoryGetSizeZ, &
                                           memoryGetAllocatedSizeX, memoryGetAllocatedSizeY, memoryGetAllocatedSizeZ, &
                                           memoryDestructor, memorySetFirstIndex, memorySetAllocatedSize, &
                                           memoryGetIncreaseSizeX, memoryGetIncreaseSizeY, memoryGetIncreaseSizeZ, &
                                           memorySetSize, memoryAllocateMemory, memoryFirstAllocateMemory, &
                                           memoryGetAllocationStatus, memoryDefineLastIndex
   USE moduleValuesArrayManagement, ONLY : memoryGetValue

   INCLUDE 'ioParameter.h'

! Declaration
! ===========

!  General part
!  ------------
   VARType, DIMENSION(:,:,:), PRIVATE, POINTER :: internalWorkingValues

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryArrayCreate, memoryPrintInformation, memoryAllocateArray
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
      INTEGER :: newSizeX, newSizeY, newSizeZ, istartValueX, istartValueY, istartValueZ, istartX, istartY, istartZ
      INTEGER, DIMENSION(3) :: istartTab

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartTab = lbound(workingArray%values)
            istartX = istartTab(1)
            istartY = istartTab(2)
            istartZ = istartTab(3)
            istartValueX = memoryGetFirstIndexX()
            istartValueY = memoryGetFirstIndexY()
            istartValueZ = memoryGetFirstIndexZ()

            IF ((memoryGetSizeX() >= memoryGetAllocatedSizeX()).OR.(istartValueX<istartX) &
                .OR. &
                (memoryGetSizeY() >= memoryGetAllocatedSizeY()).OR.(istartValueY<istartY)&
                .OR. &
                (memoryGetSizeZ() >= memoryGetAllocatedSizeZ()).OR.(istartValueZ<istartZ)) THEN
                newSizeX = memoryGetSizeX()
                newSizeY = memoryGetSizeY()
                newSizeZ = memoryGetSizeZ()
                CALL memoryStockIntermediateArray()
                CALL memoryDestructor()
                CALL memorySetFirstIndex(istartValueX,istartValueY,istartValueZ)
                CALL memorySetAllocatedSize(newSizeX+memoryGetIncreaseSizeX(),newSizeY+memoryGetIncreaseSizeY(), &
                                            newSizeZ+memoryGetIncreaseSizeZ())
                CALL memorySetSize(newSizeX,newSizeY,newSizeZ)
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
      INTEGER :: i1, i2, i3, istartX,iendX, istartY,iendY, istartZ,iendZ
      INTEGER, DIMENSION(3) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(workingArray%values)
      iendTab = ubound(workingArray%values)
      istartX = istartTab(1)
      istartY = istartTab(2)
      istartZ = istartTab(3)
      iendX = iendTab(1)
      iendY = iendTab(2)
      iendZ = iendTab(3)

      ALLOCATE(internalWorkingValues(istartX:iendX,istartY:iendY,istartZ:iendZ))

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         internalWorkingValues(i1,i2,i3) = workingArray%values(i1,i2,i3)
        END DO
       END DO
      END DO

  END SUBROUTINE

! Procedure 4 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateArrayToArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX,iendX, istartY,iendY, istartZ,iendZ
      INTEGER, DIMENSION(3) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingValues)
      iendTab = ubound(internalWorkingValues)
      istartX = istartTab(1)
      istartY = istartTab(2)
      istartZ = istartTab(3)
      iendX = iendTab(1)
      iendY = iendTab(2)
      iendZ = iendTab(3)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         workingArray%values(i1,i2,i3) = internalWorkingValues(i1,i2,i3)
        END DO
       END DO
      END DO

      DEALLOCATE(internalWorkingValues)

  END SUBROUTINE

! Procedure 5 : print information
! -------------------------------
   SUBROUTINE memoryPrintInformation()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX,iendX, istartY,iendY, istartZ,iendZ

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()


      WRITE(stdOutput,*) 'The size of the array is : ', memoryGetSizeX(), ' ', memoryGetSizeY(), ' ', memoryGetSizeZ()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX(), ' ',  memoryGetAllocatedSizeY() &
                                                                                   , ' ',  memoryGetAllocatedSizeZ()
      WRITE(stdOutput,*) '   The increase allocation memory is : ', memoryGetIncreaseSizeX(),' ', memoryGetIncreaseSizeY() &
                                                                                            ,' ', memoryGetIncreaseSizeZ()
      WRITE(stdOutput,*) '   Allocation status of the vector : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetFirstIndexX(),' ', memoryGetFirstIndexY()  &
                                                                            ,' ', memoryGetFirstIndexZ()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetLastIndexX() ,' ',&
                                                      memoryGetLastIndexY() ,' ',&
                                                      memoryGetLastIndexZ()

      IF (memoryGetAllocationStatus()) THEN
         DO i1 = istartX, iendX
          DO i2 = istartY, iendY
           DO i3 = istartZ, iendZ
            WRITE(stdOutput,*) 'value(',i1,',',i2,',',i3,') = ', memoryGetValue(i1,i2,i3)
           ENDDO
          ENDDO
         ENDDO
      END IF

   END SUBROUTINE

END MODULE moduleMemoryArray3DManagement

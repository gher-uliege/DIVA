MODULE templateBasicArray

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
   USE mathDynamicMemory
   USE templatearrayType
   
! Declaration
! ===========

!  General part
!  ------------
   TYPE (arrayType), PRIVATE, POINTER :: workingArray => NULL()

!  Memory part
!  -----------
   INTEGER, PRIVATE, PARAMETER :: defaultStartingValueX = 1
   INTEGER, PRIVATE, PARAMETER :: defaultStartingValueY = 1
   INTEGER, PRIVATE, PARAMETER :: defaultStartingValueZ = 1
   TYPE (arrayType), PRIVATE :: internalWorkingArray

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: printInformation, arrayDestroy, arraySetSize, arrayGetSizeX, arrayGetSizeY, arrayGetSizeZ, arraySetToZero, &
             arraySetToValue, arrayMin, arrayMax, arrayInsertValue, arrayAddValue, arrayGetValue, &
             arrayCreateBase, arrayCreateWithDimension, arrayCreateWithDimensionAndStartingPoint, arrayGetValues, &
             arrayGetStartIndexX, arrayGetStartIndexY, arrayGetEndIndexX, arrayGetEndIndexY, arrayGetStartIndexZ, &
             arrayGetEndIndexZ, setWorkingArray, nullify

!  Memory part
!  -----------
   PUBLIC ::  memoryGetSizeX,  memoryGetStartingPointX, memoryGetFinalValuePosition, memoryGetValues, &
              memoryGetSizeY,  memoryGetStartingPointY, &
              memoryGetSizeZ,  memoryGetStartingPointZ
   PRIVATE ::  memorySetSize, memoryAllocateArray, memoryDestructor, &
              memoryPrintInformation, memorySetAllocatedSize, memorySetAllocated, memoryAllocateMemory, &
              memoryGetAllocatedSizeX, memoryGetAllocatedSizeY, memoryGetAllocatedSizeZ, &
              memoryStockIntermediateArray, memoryTransferIntermediateArrayToArray, memoryGetValue, memoryGetAllocationStatus, &
              memoryArrayCreate, memoryGetPointerOnValue, memorySetStartingPoint

!  Access part
!  -----------
   PRIVATE ::  accessArraySetToZero, accessArraySetToValue, accessArrayInsertValue, accessArrayAddValue
   
!  Mathematic part
!  ---------------
   PRIVATE ::  mathArrayMin, mathArrayMax

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


! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures  : general                ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PUBLIC")             ===
! ============================================================

! Procedure 1 : setting pointer to array
! ---------------------------------------
   SUBROUTINE setWorkingArray(targetArray)

!     Declaration
!     - - - - - -
      TYPE(arrayType), INTENT(IN), TARGET :: targetArray

!     Body
!     - - -
      workingArray => targetArray

   END SUBROUTINE

! Procedure 2 : make the target of the pointer null
! --------------------------------------------------
   SUBROUTINE nullify()

!     Body
!     - - -
      workingArray => NULL()

   END SUBROUTINE

! Procedure 3 : create the array (only array pointer)
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
      CALL nullify()

   END SUBROUTINE

! Procedure 4 : create the array (with dimension)
! -------------------------------
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
      CALL nullify()

   END SUBROUTINE

! Procedure 5 : create the array (with dimension and istartingValue)
! -------------------------------
   SUBROUTINE arrayCreateWithDimensionAndStartingPoint(targetArray, sizeX, sizeY, sizeZ, istartingValueX, istartingValueY, &
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
      CALL memorySetStartingPoint(istartingValueX,istartingValueY,istartingValueZ)
      CALL memorySetSize(sizeX,sizeY,sizeZ)
      CALL memoryAllocateArray()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 6 : get reference to pointer containing the values
! ------------------------------------------------------------

   FUNCTION arrayGetValues(targetArray) RESULT(ptr)

!     Declaration
!     - - - - - -
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      ptr => memoryGetValues()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END FUNCTION

! Procedure 7 : print information on the array
! ---------------------------------------------
   SUBROUTINE printInformation(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryPrintInformation()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 8 : destruction of the array
! ---------------------------------------
   SUBROUTINE arrayDestroy(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL memoryDestructor()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 9 : define the size of the array
! -------------------------------------------
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
      CALL nullify()

  END SUBROUTINE

! Procedure 10 : get the size X of the array
! -------------------------------------------
  FUNCTION arrayGetSizeX(targetArray) RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      dim = memoryGetSizeX()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 11 : get the size Y of the array
! -------------------------------------------
  FUNCTION arrayGetSizeY(targetArray) RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      dim = memoryGetSizeY()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 12 : get the size Z of the array
! -------------------------------------------
  FUNCTION arrayGetSizeZ(targetArray) RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      dim = memoryGetSizeZ()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 13 : set 0 to each entry
! ---------------------------------
  SUBROUTINE arraySetToZero(targetArray)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArraySetToZero()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 14 : set "value" to each entry
! ---------------------------------------
  SUBROUTINE arraySetToValue(targetArray,val)

!     Declaration
!     - - - - - -
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArraySetToValue(val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 15 : min value
! -----------------------
  FUNCTION arrayMin(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
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
      CALL nullify()

  END FUNCTION

! Procedure 16 : max value
! -----------------------
  FUNCTION arrayMax(targetArray) RESULT(val)

!     Declaration
!     - - - - - -
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
      CALL nullify()

  END FUNCTION

! Procedure 17 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE arrayInsertValue(targetArray,positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArrayInsertValue(positionX, positionY, positionZ, val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 18 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE arrayAddValue(targetArray,positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArrayAddValue(positionX, positionY, positionZ,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 19 : get the value in the array
! ------------------------------------------
  FUNCTION arrayGetValue(targetArray,positionX,positionY,positionZ) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      val = memoryGetValue(positionX, positionY, positionZ)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END FUNCTION

! Procedure 20 : get start index
! ------------------------------
  FUNCTION arrayGetStartIndexX(targetArray) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetStartingPointX()

  END FUNCTION

! Procedure 21 : get start index
! ------------------------------
  FUNCTION arrayGetStartIndexY(targetArray) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetStartingPointY()

  END FUNCTION

! Procedure 22 : get start index
! ------------------------------
  FUNCTION arrayGetStartIndexZ(targetArray) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetStartingPointZ()

  END FUNCTION

! Procedure 23 : get end index
! ------------------------------
  FUNCTION arrayGetEndIndexX(targetArray,istart) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: istart
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetFinalValuePosition(memoryGetSizeX(),istart)

  END FUNCTION

! Procedure 24 : get end index
! ------------------------------
  FUNCTION arrayGetEndIndexY(targetArray,istart) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: istart
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetFinalValuePosition(memoryGetSizeY(),istart)

  END FUNCTION

! Procedure 25 : get end index
! ------------------------------
  FUNCTION arrayGetEndIndexZ(targetArray,istart) RESULT(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: istart
      INTEGER :: i1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      i1 = memoryGetFinalValuePosition(memoryGetSizeZ(),istart)

  END FUNCTION

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures  : memory                 ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PUBLIC")             ===
! ============================================================


! Procedure 1 : define the size of the array
! -------------------------------------------
   SUBROUTINE memorySetSize(ivalue1,ivalue2,ivalue3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalue1,ivalue2,ivalue3

!     Body
!     - - -
      workingArray%nbOfDataX = ivalue1
      workingArray%nbOfDataY = ivalue2
      workingArray%nbOfDataZ = ivalue3

   END SUBROUTINE

! Procedure 2 : define the allocated size of the array
! ------------------------------------------------------
   SUBROUTINE memorySetAllocatedSize(ivalue1,ivalue2,ivalue3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalue1,ivalue2,ivalue3

!     Body
!     - - -
      workingArray%allocatedSizeX = ivalue1
      workingArray%allocatedSizeY = ivalue2
      workingArray%allocatedSizeZ = ivalue3

   END SUBROUTINE

! Procedure 3 : define the allocation status of the array
! --------------------------------------------------------
   SUBROUTINE memorySetAllocated(icheck)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: icheck

!     Body
!     - - -
      workingArray%isAllocated = icheck

   END SUBROUTINE

! Procedure 4 : allocated memory to the array
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
            istartValueX = memoryGetStartingPointX()
            istartValueY = memoryGetStartingPointY()
            istartValueZ = memoryGetStartingPointZ()

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
                CALL memorySetStartingPoint(istartValueX,istartValueY,istartValueZ)
                CALL memorySetAllocatedSize(newSizeX+memoryGetDefaultIncreaseSizeX(),newSizeY+memoryGetDefaultIncreaseSizeY(), &
                                            newSizeZ+memoryGetDefaultIncreaseSizeZ())
                CALL memorySetSize(newSizeX,newSizeY,newSizeZ)
                CALL memoryAllocateMemory()
                CALL memoryTransferIntermediateArrayToArray()
            END IF
         CASE (.FALSE.)
            CALL memoryFirstAllocateMemory()
      END SELECT

   END SUBROUTINE

! Procedure 5 : allocated memory to the array
! ---------------------------------------------
  SUBROUTINE memoryAllocateMemory()

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      
!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetAllocatedSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetAllocatedSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetAllocatedSizeZ(),istartZ)

      ALLOCATE(workingArray%values(istartX:iendX,istartY:iendY,istartZ:iendZ))
      CALL memorySetAllocated(true)

  END SUBROUTINE

! Procedure 6 : allocated memory to the array
! ---------------------------------------------
  SUBROUTINE memoryFirstAllocateMemory()

!     Body
!     - - -
      CALL memorySetAllocatedSize(ione,ione,ione)
      CALL memoryAllocateMemory()

  END SUBROUTINE

! Procedure 7 : getting the allocated memory size
! ------------------------------------------------
  FUNCTION memoryGetAllocatedSizeX() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%allocatedSizeX

   END FUNCTION

! Procedure 8 : getting the allocated memory size
! ------------------------------------------------
  FUNCTION memoryGetAllocatedSizeY() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%allocatedSizeY

   END FUNCTION

! Procedure 9 : getting the allocated memory size
! ------------------------------------------------
  FUNCTION memoryGetAllocatedSizeZ() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%allocatedSizeZ

   END FUNCTION

! Procedure 10 : getting the array size
! --------------------------------------
  FUNCTION memoryGetSizeX() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%nbOfDataX

   END FUNCTION

! Procedure 11 : getting the array size
! --------------------------------------
  FUNCTION memoryGetSizeY() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%nbOfDataY

   END FUNCTION

! Procedure 12 : getting the array size
! --------------------------------------
  FUNCTION memoryGetSizeZ() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%nbOfDataZ

   END FUNCTION

! Procedure 13 : transfer data from workingArray to secondworkingArray
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

      ALLOCATE(internalWorkingArray%values(istartX:iendX,istartY:iendY,istartZ:iendZ))

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         internalWorkingArray%values(i1,i2,i3) = workingArray%values(i1,i2,i3)
        END DO
       END DO
      END DO

  END SUBROUTINE

! Procedure 14 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateArrayToArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX,iendX, istartY,iendY, istartZ,iendZ
      INTEGER, DIMENSION(3) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingArray%values)
      iendTab = ubound(internalWorkingArray%values)
      istartX = istartTab(1)
      istartY = istartTab(2)
      istartZ = istartTab(3)
      iendX = iendTab(1)
      iendY = iendTab(2)
      iendZ = iendTab(3)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         workingArray%values(i1,i2,i3) = internalWorkingArray%values(i1,i2,i3)
        END DO
       END DO
      END DO

      DEALLOCATE(internalWorkingArray%values)

  END SUBROUTINE

! Procedure 15 : deallocation of the memory
! ------------------------------------------
  SUBROUTINE memoryDestructor()

!     Body
!     - - -
      DEALLOCATE(workingArray%values)
      workingArray%values => NULL()
      CALL memorySetSize(izero,izero,izero)
      CALL memorySetAllocatedSize(izero,izero,izero)
      CALL memorySetStartingPoint(ione,ione,ione)
      CALL memorySetAllocated(false)

  END SUBROUTINE

! Procedure 16 : get the value in the array
! ------------------------------------------
  FUNCTION memoryGetValue(i1,i2,i3) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2,i3
      VARType :: val

!     Body
!     - - -
      val = workingArray%values(i1,i2,i3)

  END FUNCTION

! Procedure 17 : get the allocation status
! ----------------------------------------
  FUNCTION memoryGetAllocationStatus() RESULT(status)

!     Declaration
!     - - - - - -
      LOGICAL :: status

!     Body
!     - - -
      status = workingArray%isAllocated

  END FUNCTION

! Procedure 18 : print information on the vector
! ---------------------------------------------
   SUBROUTINE memoryPrintInformation()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX,iendX, istartY,iendY, istartZ,iendZ

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)


      WRITE(stdOutput,*) 'The size of the array is : ', memoryGetSizeX(), ' ', memoryGetSizeY(), ' ', memoryGetSizeZ()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX(), ' ',  memoryGetAllocatedSizeY() &
                                                                                   , ' ',  memoryGetAllocatedSizeZ()
      WRITE(stdOutput,*) '   Allocation status of the vector : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetStartingPointX(),' ', memoryGetStartingPointY()  &
                                                                               ,' ', memoryGetStartingPointZ()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetFinalValuePosition(memoryGetSizeX(),memoryGetStartingPointX()),' ',&
                                                      memoryGetFinalValuePosition(memoryGetSizeY(),memoryGetStartingPointY()),' ',&
                                                      memoryGetFinalValuePosition(memoryGetSizeZ(),memoryGetStartingPointZ())

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

! Procedure 19 : create the array
! ---------------------------------
   SUBROUTINE memoryArrayCreate()

!     Body
!     - - -
      CALL memorySetStartingPoint(defaultStartingValueX,defaultStartingValueY,defaultStartingValueZ)
      CALL memorySetSize(izero,izero,izero)
      CALL memorySetAllocatedSize(memoryGetDefaultIncreaseSizeX(),memoryGetDefaultIncreaseSizeY(),memoryGetDefaultIncreaseSizeZ())
      CALL memorySetAllocated(false)
      CALL memoryAllocateArray()

   END SUBROUTINE

! Procedure 20 : get the pointer on a value
! -----------------------------------------
  FUNCTION memoryGetPointerOnValue(positionX,positionY,positionZ) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX,positionY,positionZ
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values(positionX,positionY,positionZ)

  END FUNCTION
  
! Procedure 21 : set the starting point of the array
! ---------------------------------------------------
  SUBROUTINE memorySetStartingPoint(ivalueX,ivalueY,ivalueZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalueX,ivalueY,ivalueZ

!     Body
!     - - -
      workingArray%startValueX = ivalueX
      workingArray%startValueY = ivalueY
      workingArray%startValueZ = ivalueZ

  END SUBROUTINE
  
! Procedure 22 : get the starting point of the array
! ---------------------------------------------------
  FUNCTION memoryGetStartingPointX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%startValueX

  END FUNCTION

! Procedure 23 : get the starting point of the array
! ---------------------------------------------------
  FUNCTION memoryGetStartingPointY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%startValueY

  END FUNCTION

! Procedure 24 : get the starting point of the array
! ---------------------------------------------------
  FUNCTION memoryGetStartingPointZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%startValueZ

  END FUNCTION

! Procedure 25 : get the final position in the array with respect to given dimension
! -----------------------------------------------------------------------------------
  FUNCTION memoryGetFinalValuePosition(dim, start) RESULT(ivalue)
  
!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: dim, start
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = dim + start - 1

  END FUNCTION

! Procedure 26 : get reference to pointer containing the values
! ------------------------------------------------------------

   FUNCTION memoryGetValues() RESULT(ptr)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values

   END FUNCTION

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures : access                  ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PUBLIC")             ===
! ============================================================


! Procedure 1 : set 0 to each entry
! ---------------------------------
  SUBROUTINE accessArraySetToZero()

!     Declaration
!     - - - - - -
      VARType, PARAMETER :: val = 0

!     Body
!     - - -
      CALL accessArraySetToValue(val)

  END SUBROUTINE

! Procedure 2 : set "value" to each entry
! ---------------------------------------
  SUBROUTINE accessArraySetToValue(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, DIMENSION(:,:,:), POINTER :: ptr
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      istartZ = memoryGetStartingPointZ()
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

      ptr =>  memoryGetValues()

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
           ptr(i1,i2,i3) = val
        END DO
       END DO
      END DO

  END SUBROUTINE

! Procedure 3 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE accessArrayInsertValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      istartZ = memoryGetStartingPointZ()
      
      IF ( ( positionX < istartX ).OR.( positionY < istartY).OR.( positionZ < istartZ) ) THEN
         CALL memorySetStartingPoint(positionX,positionY,positionZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY(), &
                            istartZ-positionZ+memoryGetSizeZ())
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF
      
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

      IF ( ( positionX > iendX ).OR.( positionY > iendY ).OR.( positionZ > iendZ ) ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY + memoryGetSizeY(), &
                            positionZ-iendZ + memoryGetSizeZ())
         CALL memoryAllocateArray()
      ENDIF

30    CONTINUE
      workingArray%values(positionX,positionY,positionZ) = val

  END SUBROUTINE

! Procedure 4 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE accessArrayAddValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, INTENT(IN) :: val
      VARType, POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      IF ( positionX < istartX ) RETURN
      istartY = memoryGetStartingPointY()
      IF ( positionY < istartY ) RETURN
      istartZ = memoryGetStartingPointZ()
      IF ( positionZ < istartZ ) RETURN

      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      IF ( positionX > iendX ) RETURN
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      IF ( positionY > iendY ) RETURN
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)
      IF ( positionZ > iendZ ) RETURN

      ptr => memoryGetPointerOnValue(positionX,positionY,positionZ)
      ptr = ptr + val

  END SUBROUTINE

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===          Module procedures : mathematic              ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PUBLIC")             ===
! ============================================================

! Procedure 1 : min value
! -----------------------
  FUNCTION mathArrayMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      istartZ = memoryGetStartingPointZ()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

      ptr =>  memoryGetValues()
      
      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = min(val,ptr(i1,i2,i3))
        END DO
       END DO
      END DO

  END FUNCTION

! Procedure 2 : min value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      istartZ = memoryGetStartingPointZ()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      iendZ = memoryGetFinalValuePosition(memoryGetSizeZ(),istartZ)

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY,istartZ)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
        DO i3 = istartZ , iendZ
         val = max(val,ptr(i1,i2,i3))
        END DO
       END DO
      END DO

  END FUNCTION

END MODULE templateBasicArray


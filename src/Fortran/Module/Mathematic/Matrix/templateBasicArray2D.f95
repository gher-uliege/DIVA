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
   TYPE (arrayType), PRIVATE :: internalWorkingArray

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: printInformation, arrayDestroy, arraySetSize, arrayGetSizeX, arrayGetSizeY, arraySetToZero, arraySetToValue, &
             arrayMin, arrayMax, arrayInsertValue, arrayAddValue, arrayGetValue, &
             arrayCreateBase, arrayCreateWithDimension, arrayCreateWithDimensionAndStartingPoint, arrayGetValues, &
             arrayGetStartIndexX, arrayGetStartIndexY, arrayGetEndIndexX, arrayGetEndIndexY, setWorkingArray, nullify, &
             arraySetIncreaseSize

!  Memory part
!  -----------
   PUBLIC ::  memoryGetSizeX,  memoryGetStartingPointX, memoryGetFinalValuePosition, memoryGetValues, &
              memoryGetSizeY,  memoryGetStartingPointY
   PRIVATE ::  memorySetSize, memoryAllocateArray, memoryDestructor, &
              memoryPrintInformation, memorySetAllocatedSize, memorySetAllocated, memoryAllocateMemory, &
              memoryGetAllocatedSizeX, memoryGetAllocatedSizeY, &
              memoryStockIntermediateArray, memoryTransferIntermediateArrayToArray, memoryGetValue, memoryGetAllocationStatus, &
              memoryArrayCreate, memoryGetPointerOnValue, memorySetStartingPoint, memoryGetIncreaseSizeX, memorySetIncreaseSize, &
              memoryGetIncreaseSizeY

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
      CALL nullify()

   END SUBROUTINE

! Procedure 5 : create the array (with dimension and istartingValue)
! -------------------------------
   SUBROUTINE arrayCreateWithDimensionAndStartingPoint(targetArray, sizeX, sizeY, istartingValueX, istartingValueY)

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
      CALL memorySetStartingPoint(istartingValueX,istartingValueY)
      CALL memorySetSize(sizeX,sizeY)
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
      VARType, DIMENSION(:,:), POINTER :: ptr

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

! Procedure 9 : destruction of the array
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

! Procedure 12 : set 0 to each entry
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

! Procedure 13 : set "value" to each entry
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

! Procedure 14 : min value
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

! Procedure 15 : max value
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

! Procedure 16 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE arrayInsertValue(targetArray,positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArrayInsertValue(positionX, positionY,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 17 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE arrayAddValue(targetArray,positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      VARType, INTENT(IN) :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

!     Body
!     - - -
      CALL accessArrayAddValue(positionX, positionY,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

  END SUBROUTINE

! Procedure 18 : get the value in the array
! ------------------------------------------
  FUNCTION arrayGetValue(targetArray,i1,i2) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
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
      CALL nullify()

  END FUNCTION

! Procedure 19 : get start index
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

! Procedure 20 : get start index
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

! Procedure 21 : get end index
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

! Procedure 22 : get end index
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

! Procedure 23 : define the increase size
! ----------------------------------------
  SUBROUTINE arraySetIncreaseSize(targetArray,sizeX,sizeY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX, sizeY
      INTEGER :: dimX, dimY

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetArray
      CALL setWorkingArray(targetArray)

      dimX = sizeX
      dimY = sizeY

      IF ( sizeX < 0 ) THEN
         dimX = memoryGetDefaultIncreaseSizeX()
      END IF
      IF ( sizeY < 0 ) THEN
         dimY = memoryGetDefaultIncreaseSizeY()
      END IF

      CALL memorySetIncreaseSize(dimX,dimY)

  END SUBROUTINE

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
   SUBROUTINE memorySetSize(ivalue1,ivalue2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalue1,ivalue2

!     Body
!     - - -
      workingArray%nbOfDataX = ivalue1
      workingArray%nbOfDataY = ivalue2

   END SUBROUTINE

! Procedure 2 : define the allocated size of the array
! ------------------------------------------------------
   SUBROUTINE memorySetAllocatedSize(ivalue1,ivalue2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalue1,ivalue2

!     Body
!     - - -
      workingArray%allocatedSizeX = ivalue1
      workingArray%allocatedSizeY = ivalue2

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
      INTEGER :: newSizeX, newSizeY, istartValueX, istartValueY, istartX, istartY
      INTEGER, DIMENSION(2) :: istartTab

!     Body
!     - - -
      SELECT CASE (memoryGetAllocationStatus())
         CASE (.TRUE.)
            istartTab = lbound(workingArray%values)
            istartX = istartTab(1)
            istartY = istartTab(2)
            istartValueX = memoryGetStartingPointX()
            istartValueY = memoryGetStartingPointY()

            IF ((memoryGetSizeX() >= memoryGetAllocatedSizeX()).OR.(istartValueX<istartX) &
                .OR. &
                (memoryGetSizeY() >= memoryGetAllocatedSizeY()).OR.(istartValueY<istartY)) THEN
                newSizeX = memoryGetSizeX()
                newSizeY = memoryGetSizeY()
                CALL memoryStockIntermediateArray()
                CALL memoryDestructor()
                CALL memorySetStartingPoint(istartValueX,istartValueY)
                CALL memorySetAllocatedSize(newSizeX+memoryGetIncreaseSizeX(),newSizeY+memoryGetIncreaseSizeY())
                CALL memorySetSize(newSizeX,newSizeY)
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
      INTEGER :: istartX, iendX, istartY, iendY
      
!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetAllocatedSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetAllocatedSizeY(),istartY)

      ALLOCATE(workingArray%values(istartX:iendX,istartY:iendY))
      CALL memorySetAllocated(true)

  END SUBROUTINE

! Procedure 6 : allocated memory to the array
! ---------------------------------------------
  SUBROUTINE memoryFirstAllocateMemory()

!     Body
!     - - -
      CALL memorySetAllocatedSize(ione,ione)
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

! Procedure 9 : getting the array size
! --------------------------------------
  FUNCTION memoryGetSizeX() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%nbOfDataX

   END FUNCTION

! Procedure 10 : getting the array size
! --------------------------------------
  FUNCTION memoryGetSizeY() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%nbOfDataY

   END FUNCTION

! Procedure 11 : transfer data from workingArray to secondworkingArray
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

      ALLOCATE(internalWorkingArray%values(istartX:iendX,istartY:iendY))

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         internalWorkingArray%values(i1,i2) = workingArray%values(i1,i2)
       END DO
      END DO

  END SUBROUTINE

! Procedure 12 : transfer data from secondworkingArray to workingArray
! -----------------------------------------------------------------------
  SUBROUTINE memoryTransferIntermediateArrayToArray()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX,iendX, istartY,iendY
      INTEGER, DIMENSION(2) :: istartTab,iendTab

!     Body
!     - - -
      istartTab = lbound(internalWorkingArray%values)
      iendTab = ubound(internalWorkingArray%values)
      istartX = istartTab(1)
      istartY = istartTab(2)
      iendX = iendTab(1)
      iendY = iendTab(2)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         workingArray%values(i1,i2) = internalWorkingArray%values(i1,i2)
       END DO
      END DO

      DEALLOCATE(internalWorkingArray%values)

  END SUBROUTINE

! Procedure 13 : deallocation of the memory
! ------------------------------------------
  SUBROUTINE memoryDestructor()

!     Body
!     - - -
      DEALLOCATE(workingArray%values)
      workingArray%values => NULL()
      CALL memorySetSize(izero,izero)
      CALL memorySetAllocatedSize(izero,izero)
      CALL memorySetIncreaseSize(memoryGetDefaultIncreaseSizeX(),memoryGetDefaultIncreaseSizeY())
      CALL memorySetStartingPoint(ione,ione)
      CALL memorySetAllocated(false)

  END SUBROUTINE

! Procedure 14 : get the value in the array
! ------------------------------------------
  FUNCTION memoryGetValue(i1,i2) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
      VARType :: val

!     Body
!     - - -
      val = workingArray%values(i1,i2)

  END FUNCTION

! Procedure 15 : get the allocation status
! ----------------------------------------
  FUNCTION memoryGetAllocationStatus() RESULT(status)

!     Declaration
!     - - - - - -
      LOGICAL :: status

!     Body
!     - - -
      status = workingArray%isAllocated

  END FUNCTION

! Procedure 16 : print information on the vector
! ---------------------------------------------
   SUBROUTINE memoryPrintInformation()

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)


      WRITE(stdOutput,*) 'The size of the array is : ', memoryGetSizeX(), ' ', memoryGetSizeY()
      WRITE(stdOutput,*) '   The allocated memory is : ', memoryGetAllocatedSizeX(), ' ',  memoryGetAllocatedSizeY()
      WRITE(stdOutput,*) '   The increase allocation memory is : ', memoryGetIncreaseSizeX(),' ', memoryGetIncreaseSizeY()
      WRITE(stdOutput,*) '   Allocation status of the vector : ', memoryGetAllocationStatus()
      WRITE(stdOutput,*) '   First positions are : ', memoryGetStartingPointX(),' ', memoryGetStartingPointY()
      WRITE(stdOutput,*) '   Last positions are  : ', memoryGetFinalValuePosition(memoryGetSizeX(),memoryGetStartingPointX()),' ',&
                                                      memoryGetFinalValuePosition(memoryGetSizeY(),memoryGetStartingPointY())

      IF (memoryGetAllocationStatus()) THEN
         DO i1 = istartX, iendX
          DO i2 = istartY, iendY
            WRITE(stdOutput,*) 'value(',i1,',',i2,') = ', memoryGetValue(i1,i2)
          ENDDO
         ENDDO
      END IF

   END SUBROUTINE

! Procedure 17 : create the array
! ---------------------------------
   SUBROUTINE memoryArrayCreate()

!     Body
!     - - -
      CALL memorySetStartingPoint(defaultStartingValueX,defaultStartingValueY)
      CALL memorySetSize(izero,izero)
      CALL memorySetIncreaseSize(memoryGetDefaultIncreaseSizeX(),memoryGetDefaultIncreaseSizeY())
      CALL memorySetAllocatedSize(memoryGetIncreaseSizeX(),memoryGetIncreaseSizeY())
      CALL memorySetAllocated(false)
      CALL memoryAllocateArray()

   END SUBROUTINE

! Procedure 18 : get the pointer on a value
! -----------------------------------------
  FUNCTION memoryGetPointerOnValue(positionX,positionY) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX,positionY
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values(positionX,positionY)

  END FUNCTION
  
! Procedure 19 : set the starting point of the array
! ---------------------------------------------------
  SUBROUTINE memorySetStartingPoint(ivalueX,ivalueY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: ivalueX,ivalueY

!     Body
!     - - -
      workingArray%startValueX = ivalueX
      workingArray%startValueY = ivalueY

  END SUBROUTINE
  
! Procedure 20: get the starting point of the array
! ---------------------------------------------------
  FUNCTION memoryGetStartingPointX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%startValueX

  END FUNCTION

! Procedure 21 : get the starting point of the array
! ---------------------------------------------------
  FUNCTION memoryGetStartingPointY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%startValueY

  END FUNCTION

! Procedure 22 : get the final position in the array with respect to given dimension
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

! Procedure 23 : get reference to pointer containing the values
! ------------------------------------------------------------

   FUNCTION memoryGetValues() RESULT(ptr)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values

   END FUNCTION

! Procedure 24 : set the increase size for memory allocation
! ----------------------------------------------------------
  SUBROUTINE memorySetIncreaseSize(sizeX,sizeY)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: sizeX,sizeY

!     Body
!     - - -
      workingArray%increaseSizeX = sizeX
      workingArray%increaseSizeY = sizeY

  END SUBROUTINE

! Procedure 25 : get the increase size for memory allocation
! ----------------------------------------------------------
  FUNCTION memoryGetIncreaseSizeX() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%increaseSizeX

  END FUNCTION

! Procedure 25 : get the increase size for memory allocation
! ----------------------------------------------------------
  FUNCTION memoryGetIncreaseSizeY() RESULT(size)

!     Declaration
!     - - - - - -
      INTEGER :: size

!     Body
!     - - -
      size = workingArray%increaseSizeY

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
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType, DIMENSION(:,:), POINTER :: ptr
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      istartY = memoryGetStartingPointY()
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)

      ptr =>  memoryGetValues()

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
           ptr(i1,i2) = val
       END DO
      END DO

  END SUBROUTINE

! Procedure 3 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE accessArrayInsertValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX,positionY
      INTEGER :: istartX, iendX, istartY, iendY
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      IF ( ( positionX < istartX ).OR.( positionY < istartY) ) THEN
         CALL memorySetStartingPoint(positionX,positionY)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY())
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF
      
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      
      IF ( ( positionX > iendX ).OR.( positionY > iendY ) ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY + memoryGetSizeY())
         CALL memoryAllocateArray()
      ENDIF

30    CONTINUE
      workingArray%values(positionX,positionY) = val

  END SUBROUTINE

! Procedure 4 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE accessArrayAddValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX,positionY
      INTEGER :: istartX, iendX, istartY, iendY
      VARType, INTENT(IN) :: val
      VARType, POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      IF ( positionX < istartX ) RETURN
      istartY = memoryGetStartingPointY()
      IF ( positionY < istartY ) RETURN

      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      IF ( positionX > iendX ) RETURN
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)
      IF ( positionY > iendY ) RETURN

      ptr => memoryGetPointerOnValue(positionX,positionY)
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
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)

      ptr =>  memoryGetValues()
      
      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = min(val,ptr(i1,i2))
       END DO
      END DO

  END FUNCTION

! Procedure 2 : min value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType :: val
      VARType, DIMENSION(:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetStartingPointX()
      istartY = memoryGetStartingPointY()
      iendX = memoryGetFinalValuePosition(memoryGetSizeX(),istartX)
      iendY = memoryGetFinalValuePosition(memoryGetSizeY(),istartY)

      ptr =>  memoryGetValues()

      val = ptr(istartX,istartY)

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
         val = max(val,ptr(i1,i2))
       END DO
      END DO

  END FUNCTION

END MODULE templateBasicArray


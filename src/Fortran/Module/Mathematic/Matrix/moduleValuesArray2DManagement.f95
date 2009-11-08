MODULE moduleValuesArray2DManagement

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
  USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, memoryGetLastIndexX, &
                                          memoryGetFirstIndexY, memoryGetLastIndexY, &
                                          memorySetFirstIndex, memorySetSize, memoryDefineLastIndex, &
                                          memoryGetSizeX, memoryGetSizeY
  USE moduleMemoryArrayNDManagement, ONLY : memoryAllocateArray
  USE moduleValuesArrayManagement, ONLY : memoryGetPointerOnValue, memoryGetValues

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryArraySetToZero, memoryArraySetToValue, memoryArrayInsertValue, memoryArrayAddValue, memoryArrayFastInsertValue, &
             memoryArrayFastAddValue


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
! Procedure 1 : set 0 to each entry
! ---------------------------------
  SUBROUTINE memoryArraySetToZero()

!     Declaration
!     - - - - - -
      VARType, PARAMETER :: val = 0

!     Body
!     - - -
      CALL memoryArraySetToValue(val)

  END SUBROUTINE

! Procedure 2 : set "value" to each entry
! ---------------------------------------
  SUBROUTINE memoryArraySetToValue(val)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, istartX, iendX, istartY, iendY
      VARType, DIMENSION(:,:), POINTER :: ptr
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()

      ptr =>  memoryGetValues()

      DO i1 = istartX , iendX
       DO i2 = istartY , iendY
           ptr(i1,i2) = val
       END DO
      END DO

  END SUBROUTINE

! Procedure 3 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE memoryArrayInsertValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      INTEGER :: istartX, iendX, istartY, iendY
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      istartY = memoryGetFirstIndexY()

      IF ( ( positionX < istartX ).OR.( positionY < istartY) ) THEN
         CALL memorySetFirstIndex(positionX,positionY)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      iendX = memoryGetLastIndexX()
      iendY = memoryGetLastIndexY()

      IF ( ( positionX > iendX ).OR.( positionY > iendY ) ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY + memoryGetSizeY())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
      ENDIF

30    CONTINUE

         CALL memoryArrayFastInsertValue(positionX,positionY,val)

  END SUBROUTINE

! Procedure 4 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE memoryArrayAddValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      INTEGER :: istartX, iendX, istartY, iendY
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      IF ( positionX < istartX ) RETURN
      istartY = memoryGetFirstIndexY()
      IF ( positionY < istartY ) RETURN

      iendX = memoryGetLastIndexX()
      IF ( positionX > iendX ) RETURN
      iendY = memoryGetLastIndexY()
      IF ( positionY > iendY ) RETURN

      CALL memoryArrayFastAddValue(positionX,positionY,val)

  END SUBROUTINE

! Procedure 5 : fast insert value (no check)
! ------------------------------------------
  SUBROUTINE memoryArrayFastInsertValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      workingArray%values(positionX,positionY) = val

  END SUBROUTINE

! Procedure 6 : fast add value (no check)
! ------------------------------------------
  SUBROUTINE memoryArrayFastAddValue(positionX,positionY,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY
      VARType, INTENT(IN) :: val
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(positionX,positionY)
      ptr = ptr + val

  END SUBROUTINE

END MODULE moduleValuesArray2DManagement

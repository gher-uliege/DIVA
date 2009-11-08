MODULE moduleValuesArray3DManagement

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
                                          memoryGetFirstIndexZ, memoryGetLastIndexZ, &
                                          memorySetFirstIndex, memorySetSize, memoryDefineLastIndex, &
                                          memoryGetSizeX, memoryGetSizeY, memoryGetSizeZ
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
      INTEGER :: i1, i2, i3, istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, DIMENSION(:,:,:), POINTER :: ptr
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

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
  SUBROUTINE memoryArrayInsertValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      istartY = memoryGetFirstIndexY()
      istartZ = memoryGetFirstIndexZ()

      IF ( ( positionX < istartX ).OR.( positionY < istartY).OR.( positionZ < istartZ) ) THEN
         CALL memorySetFirstIndex(positionX,positionY,positionZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY(), &
                            istartZ-positionZ+memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      iendX = memoryGetLastIndexX()
      iendY = memoryGetLastIndexY()
      iendZ = memoryGetLastIndexZ()

      IF ( ( positionX > iendX ).OR.( positionY > iendY ).OR.( positionZ > iendZ ) ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY + memoryGetSizeY(), &
                            positionZ-iendZ + memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
      ENDIF

30    CONTINUE

         CALL memoryArrayFastInsertValue(positionX,positionY,positionZ,val)

  END SUBROUTINE

! Procedure 4 : add value in array (value = old value + new value)
! -----------------------------------------------------------------
  SUBROUTINE memoryArrayAddValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      IF ( positionX < istartX ) RETURN
      istartY = memoryGetFirstIndexY()
      IF ( positionY < istartY ) RETURN
      istartZ = memoryGetFirstIndexZ()
      IF ( positionZ < istartZ ) RETURN

      iendX = memoryGetLastIndexX()
      IF ( positionX > iendX ) RETURN
      iendY = memoryGetLastIndexY()
      IF ( positionY > iendY ) RETURN
      iendZ = memoryGetLastIndexZ()
      IF ( positionZ > iendZ ) RETURN

      CALL memoryArrayFastAddValue(positionX,positionY,positionZ,val)

  END SUBROUTINE

! Procedure 5 : fast insert value (no check)
! ------------------------------------------
  SUBROUTINE memoryArrayFastInsertValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      VARType, INTENT(IN) :: val

!     Body
!     - - -
      workingArray%values(positionX,positionY,positionZ) = val

  END SUBROUTINE

! Procedure 6 : fast add value (no check)
! ------------------------------------------
  SUBROUTINE memoryArrayFastAddValue(positionX,positionY,positionZ,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX, positionY, positionZ
      VARType, INTENT(IN) :: val
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => memoryGetPointerOnValue(positionX,positionY,positionZ)
      ptr = ptr + val

  END SUBROUTINE

END MODULE moduleValuesArray3DManagement

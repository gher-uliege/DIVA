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
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
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

      ptr(istartX:iendX,istartY:iendY,istartZ:iendZ) = val

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

      IF (( positionX < istartX ).AND.( positionY < istartY ).AND.( positionZ < istartZ )) THEN
         CALL memorySetFirstIndex(positionX,positionY,positionZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY(),&
                            istartZ-positionZ+memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionX < istartX ).AND.( positionY < istartY )) THEN
         CALL memorySetFirstIndex(positionX,positionY,istartZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),istartY-positionY+memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionX < istartX ).AND.( positionZ < istartZ )) THEN
         CALL memorySetFirstIndex(positionX,istartY,positionZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),memoryGetSizeY(),istartZ-positionZ+memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionY < istartY ).AND.( positionZ < istartZ )) THEN
         CALL memorySetFirstIndex(istartX,positionY,positionZ)
         CALL memorySetSize(memoryGetSizeX(),istartY-positionY+memoryGetSizeY(),istartZ-positionZ+memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionX < istartX ) THEN
         CALL memorySetFirstIndex(positionX,istartY,istartZ)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX(),memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionY < istartY ) THEN
         CALL memorySetFirstIndex(istartX,positionY,istartZ)
         CALL memorySetSize(memoryGetSizeX(),istartY-positionY+memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionZ < istartZ ) THEN
         CALL memorySetFirstIndex(istartX,istartY,positionZ)
         CALL memorySetSize(memoryGetSizeX(),memoryGetSizeY(),istartZ-positionZ+memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      iendX = memoryGetLastIndexX()
      iendY = memoryGetLastIndexY()
      iendZ = memoryGetLastIndexZ()

      IF (( positionX > iendX ).AND.( positionY > iendY ).AND.( positionZ > iendZ )) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY +memoryGetSizeY(),positionZ-iendZ +memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionX > iendX ).AND.( positionY > iendY )) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),positionY-iendY +memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionX > iendX ).AND.( positionZ > iendZ )) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),memoryGetSizeY(),positionZ-iendZ +memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF (( positionY > iendY ).AND.( positionZ > iendZ )) THEN
         CALL memorySetSize(memoryGetSizeX(),positionY-iendY +memoryGetSizeY(),positionZ-iendZ +memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionX > iendX ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX(),memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionY > iendY ) THEN
         CALL memorySetSize(memoryGetSizeX(),positionY-iendY + memoryGetSizeY(),memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
      ENDIF

      IF ( positionZ > iendZ ) THEN
         CALL memorySetSize(memoryGetSizeX(),memoryGetSizeY(),positionZ-iendZ + memoryGetSizeZ())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateArray()
         GOTO 30
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

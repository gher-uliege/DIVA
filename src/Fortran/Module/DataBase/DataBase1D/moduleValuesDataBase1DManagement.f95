MODULE moduleValuesDataBase1DManagement

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
  USE moduleMemoryDataBaseManagement, ONLY : memoryGetFirstIndexX, memoryGetLastIndexX, &
                                          memorySetFirstIndex, memorySetSize, memoryDefineLastIndex, &
                                          memoryGetSizeX, memoryGetAllocatedSizeX
  USE moduleMemoryDataBase1DManagement, ONLY : memoryAllocateDataBase
  USE moduleValuesDataBaseManagement, ONLY : memoryGetPointerOnValue
  USE moduleGenericTypeSurDefined, ONLY : genericTypeCopy => copy

! Declaration
! ===========

  INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryDataBaseInsertValue,memoryDataBaseFastInsertValue


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
! Procedure 1 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE memoryDataBaseInsertValue(positionX,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX
      INTEGER :: istartX, iendX
      TYPE(genericType), POINTER :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()

      IF ( ( positionX < istartX ) ) THEN
         CALL memorySetFirstIndex(positionX)
         CALL memorySetSize(istartX-positionX+memoryGetSizeX())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateDataBase()
         GOTO 30
      ENDIF

      iendX = memoryGetLastIndexX()

      IF ( ( positionX > iendX ) ) THEN
         CALL memorySetSize(positionX-iendX + memoryGetSizeX())
         CALL memoryDefineLastIndex()
         CALL memoryAllocateDataBase()
      ENDIF

30    CONTINUE

      CALL memoryDataBaseFastInsertValue(positionX,val)

  END SUBROUTINE

! Procedure 2 : fast insert value (no check)
! ------------------------------------------
  SUBROUTINE memoryDataBaseFastInsertValue(positionX,val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: positionX
      TYPE(genericType), POINTER :: val, val2

!     Body
!     - - -
      val2 => memoryGetPointerOnValue(positionX)
      CALL genericTypeCopy(val2,val)

  END SUBROUTINE


END MODULE moduleValuesDataBase1DManagement

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
  USE moduleInsertValueMethod
  USE moduleMemoryDataBaseManagement, ONLY : memoryGetFirstIndexX, memoryGetLastIndexX, &
                                          memorySetFirstIndex, memorySetSize, memoryDefineLastIndex, &
                                          memoryGetSizeX, memoryGetAllocatedSizeX
  USE moduleMemoryDataBase1DManagement, ONLY : memoryAllocateDataBase
  USE moduleValuesDataBaseManagement, ONLY : memoryGetPointerOnValue, memoryGetValues
  USE moduleGenericTypeSurDefined, ONLY : genericTypeCopy => copy, genericTypeInitialise => initialise

! Declaration
! ===========

  INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryDataBaseInsertValue,memoryDataBaseFastInsertValue, memoryDataBaseInitialise


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

! Procedure 3 : initialisation
! ----------------------------
  SUBROUTINE memoryDataBaseInitialise()

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, i1
      TYPE(genericType), DIMENSION(:), POINTER :: val

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()

      val => memoryGetValues()

      DO i1 = iStartX, iendX
          CALL genericTypeInitialise(val(i1),i1)
      END DO

  END SUBROUTINE

! Procedure 4 : global procedure to insert the value in the array at specified position
! --------------------------------------------------------------------------------------
  SUBROUTINE memoryDataBaseSetValue(i1,val,modeType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(insertValueMethod), INTENT(IN) :: modeType
      TYPE(genericType), POINTER :: val

!     Body
!     - - -
      SELECT CASE (modeType%insertValueMode)
         CASE (INSERT_VALUE%insertValueMode)
             CALL memoryDataBaseInsertValue(i1,val)
         CASE (SET_VALUE%insertValueMode)
             CALL memoryDataBaseFastInsertValue(i1,val)
         CASE DEFAULT
             CALL memoryDataBaseInsertValue(i1,val)
      END SELECT

  END SUBROUTINE

END MODULE moduleValuesDataBase1DManagement

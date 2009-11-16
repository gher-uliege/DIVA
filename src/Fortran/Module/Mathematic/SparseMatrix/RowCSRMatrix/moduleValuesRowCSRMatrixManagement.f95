MODULE moduleValuesRowCSRMatrixManagement

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
   USE moduleRowCSRMatrixDefinition
   USE moduleWorkingArray, ONLY : workingArray
   USE vectorInterface

   INCLUDE 'constantParameter.h'

! Declaration
! ===========

!  General part
!  ------------

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryGetValues, memoryGetIndex, memoryGetValue, memoryGetPointerOnValue
   PUBLIC :: memoryArraySetToZero, memoryArraySetToValue , memoryArrayInsertValue, memoryArrayAddValue
   PRIVATE :: memoryGetInternalPosition

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
! ===            Internal procedure ("PUBLIC")  : Getting   ===
! =============================================================

! Procedure 1 : get reference to pointer containing the row values
! ----------------------------------------------------------------
   FUNCTION memoryGetValues() RESULT(ptr)

!    Declaration
!    - - - - - - -
     TYPE(vectorTypeValue), POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values

   END FUNCTION

! Procedure 2 : get reference to pointer containing the row index
! ----------------------------------------------------------------
   FUNCTION memoryGetIndex() RESULT(ptr)

!    Declaration
!    - - - - - - -
     TYPE(vectorTypeIndex), POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%columnIndex

   END FUNCTION

! Procedure 4 : get the value at position i1 ,i2 in the csrmatrix
! ---------------------------------------------------------------
   FUNCTION memoryGetValue(position) RESULT(val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: position
     VARType, POINTER :: val1
     VARType :: val

!     Body
!     - - -
      val1 => memoryGetPointerOnValue(position)
      val = zero

      IF ( associated(val1) ) THEN
         val = val1
      ENDIF

   END FUNCTION


! Procedure 4 : get the value at position i1 ,i2 in the csrmatrix
! ---------------------------------------------------------------
   FUNCTION memoryGetPointerOnValue(position) RESULT(ptrVal)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: position
     INTEGER :: istart, iend, i1, indexValue
     VARType, POINTER :: ptrVal

     TYPE(vectorTypeValue), POINTER :: ptrValue
     TYPE(vectorTypeIndex), POINTER :: ptrIndex
     ROWIndexVARType, DIMENSION(:), POINTER :: ptrIndexValue

!     Body
!     - - -
      ptrValue => memoryGetValues()
      ptrIndex => memoryGetIndex()

      ptrVal => NULL()

      istart = vectorGetFirstIndex(ptrIndex)
      iend = vectorGetLastIndex(ptrIndex)
      ptrIndexValue => vectorGetValues(ptrIndex)

      DO i1 = istart, iend
        indexValue = ptrIndexValue(i1)
         IF ( indexValue == position ) THEN
             ptrVal => vectorGetPointerOnValue(ptrValue,i1)
             GOTO 30
         END IF
      END DO

30    CONTINUE

   END FUNCTION

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

!    Declaration
!    - - - - - - -
     VARType, INTENT(IN) :: val
     TYPE(vectorTypeValue), POINTER :: ptrValue

!     Body
!     - - -
      ptrValue => memoryGetValues()
      CALL vectorSetToValue(ptrValue,val)

  END SUBROUTINE


! Procedure 3 : insert value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE memoryArrayInsertValue(positionX,val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: positionX
     INTEGER :: iposition, iinsert
     VARType, INTENT(IN) :: val

     TYPE(vectorTypeValue), POINTER :: ptrValue
     TYPE(vectorTypeIndex), POINTER :: ptrIndex

!     Body
!     - - -
      ptrValue => memoryGetValues()
      ptrIndex => memoryGetIndex()

!       Look for position in the existing data
!       + + + + + + + + + + + + + + + + + + + +
      CALL memoryGetInternalPosition(ptrIndex,positionX,iposition,iinsert)

      SELECT CASE (iinsert)
         CASE (ione)
             CALL vectorPutIn(ptrIndex,iposition,positionX)
             CALL vectorPutIn(ptrValue,iposition,val)
         CASE (itwo)
             CALL vectorFastInsertValue(ptrValue,iposition,val)
         CASE (ithree)
             CALL vectorInsertValue(ptrValue,iposition,val)
             CALL vectorInsertValue(ptrIndex,iposition,positionX)
      END SELECT

  END SUBROUTINE

! Procedure 4 : search internal position
! --------------------------------------
   SUBROUTINE memoryGetInternalPosition(ptrIndex,positionX,iposition,iinsert)

!    Declaration
!    - - - - - -
     INTEGER, INTENT(OUT) :: iposition, iinsert
     INTEGER, INTENT(IN) :: positionX
     INTEGER :: istart, iend, i1
     TYPE(vectorTypeIndex), POINTER :: ptrIndex
     ROWIndexVARType, DIMENSION(:), POINTER :: ptrIndexValue

!    Body
!    - - -
      iposition = vectorGetFirstIndexX(ptrIndex)
      iend = vectorGetLastIndexX(ptrIndex)
      ptrIndexValue => vectorGetValues(ptrIndex)

      istart = iposition
      iinsert = ione

      DO i1 = istart, iend
         IF ( ptrIndexValue(i1) > positionX ) THEN
            iposition = i1
            GOTO 30
         ELSE IF ( ptrIndexValue(i1) == positionX ) THEN
            iposition = i1
            GOTO 30
            iinsert = itwo
         END IF
      ENDDO

      iposition = iend + 1
      iinsert = ithree

30    CONTINUE

   END SUBROUTINE

! Procedure 5 : add value in array (scracth the previous one)
! ---------------------------------------------------------------
  SUBROUTINE memoryArrayAddValue(positionX,val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: positionX
     INTEGER :: iposition, iinsert
     VARType, INTENT(IN) :: val

     TYPE(vectorTypeValue), POINTER :: ptrValue
     TYPE(vectorTypeIndex), POINTER :: ptrIndex

!     Body
!     - - -
      ptrValue => memoryGetValues()
      ptrIndex => memoryGetIndex()

!       Look for position in the existing data
!       + + + + + + + + + + + + + + + + + + + +
      CALL memoryGetInternalPosition(ptrIndex,positionX,iposition,iinsert)

      IF ( iinsert == itwo ) THEN
          CALL vectorFastAddValue(ptrValue,iposition,val)
      END IF

  END SUBROUTINE


END MODULE moduleValuesRowCSRMatrixManagement

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

END MODULE moduleValuesRowCSRMatrixManagement


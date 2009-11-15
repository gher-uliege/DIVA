MODULE moduleValuesCSRMatrixManagement

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
   USE moduleCSRMatrixDefinition
   USE moduleWorkingArray, ONLY : workingArray
   USE rowCSRMatrixInterface

! Declaration
! ===========

!  General part
!  ------------


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryGetRow, memoryGetRowValues, memoryGetRowIndex, memoryGetValue, memoryGetPointerOnValue

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

! Procedure 1 : get reference to pointer containing the row
! ----------------------------------------------------------
   FUNCTION memoryGetRow(i1) RESULT(ptr)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1
     TYPE(rowCSRMatrixType), POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%rowIndex(i1)

   END FUNCTION

! Procedure 2 : get reference to pointer containing the row values
! ----------------------------------------------------------------
   FUNCTION memoryGetRowValues(i1) RESULT(ptr)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1
     TYPE(vectorTypeValue), POINTER :: ptr

!     Body
!     - - -
      ptr =>  workingArray%rowIndex(i1)%values

   END FUNCTION

! Procedure 3 : get reference to pointer containing the row index
! ----------------------------------------------------------------
   FUNCTION memoryGetRowIndex(i1) RESULT(ptr)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1
     TYPE(vectorTypeIndex), POINTER :: ptr

!     Body
!     - - -
      ptr =>  workingArray%rowIndex(i1)%columnIndex

   END FUNCTION

! Procedure 4 : get the value at position i1 ,i2 in the csrmatrix
! ---------------------------------------------------------------
   FUNCTION memoryGetValue(i1,i2) RESULT(val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1, i2
     VARType :: val

!     Body
!     - - -
      val = rowCSRMatrixGetValue(memoryGetRow(i1),i2)

   END FUNCTION

! Procedure 4 : get the value at position i1 ,i2 in the csrmatrix
! ---------------------------------------------------------------
   FUNCTION memoryGetPointerOnValue(i1,i2) RESULT(val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1, i2
     VARType, POINTER :: val

!     Body
!     - - -
      val => rowCSRMatrixGetPointerOnValue(memoryGetRow(i1),i2)

   END FUNCTION

END MODULE moduleValuesCSRMatrixManagement


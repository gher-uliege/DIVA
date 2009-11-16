MODULE moduleRowCSRMatrix

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
   USE moduleWorkingArray, ONLY : setWorkingArray, nullifyArrayPointer

   USE moduleValuesRowCSRMatrixManagement, ONLY : memoryGetValues, memoryGetIndex, memoryGetValue, &
                                                  memoryGetPointerOnValue , memoryArraySetToZero, &
                                                  memoryArraySetToValue, memoryArrayInsertValue, &
                                                  memoryArrayAddValue


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: rowCSRMatrixGetValues, rowCSRMatrixGetIndex, rowCSRMatrixGetValue, rowCSRMatrixGetPointerOnValue, &
             rowCSRMatrixSetToZero, rowCSRMatrixSetToValue, rowCSRMatrixInsertValue, rowCSRMatrixAddValue

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

! Procedure 1 : get reference to pointer containing the values
! ------------------------------------------------------------
   FUNCTION rowCSRMatrixGetValues(targetRowCSRMatrix) RESULT(ptr)

!    Declaration
!    - - - - - - -
     TYPE(vectorTypeValue), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      ptr => memoryGetValues()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END FUNCTION

! Procedure 2 : get reference to pointer containing the index
! ------------------------------------------------------------
   FUNCTION rowCSRMatrixGetIndex(targetRowCSRMatrix) RESULT(ptr)

!    Declaration
!    - - - - - - -
     TYPE(vectorTypeIndex), POINTER :: ptr

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      ptr => memoryGetIndex()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END FUNCTION

! Procedure 3 : get the value at position i1
! ------------------------------------------
   FUNCTION rowCSRMatrixGetValue(targetRowCSRMatrix,i1) RESULT(val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1
     VARType :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      val = memoryGetValue(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END FUNCTION

! Procedure 4 : get the value at position i1
! ------------------------------------------
   FUNCTION rowCSRMatrixGetPointerOnValue(targetRowCSRMatrix,i1) RESULT(val)

!    Declaration
!    - - - - - - -
     INTEGER, INTENT(IN) :: i1
     VARType, POINTER :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      val => memoryGetPointerOnValue(i1)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END FUNCTION


! Procedure 5 : set to zero
! -------------------------
   SUBROUTINE rowCSRMatrixSetToZero(targetRowCSRMatrix)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      CALL memoryArraySetToZero()

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

! Procedure 6 : set to value
! ---------------------------
   SUBROUTINE rowCSRMatrixSetToValue(targetRowCSRMatrix,val)

!     Declaration
!     - - - - - - -
      VARType, POINTER :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      CALL memoryArraySetToValue(val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

! Procedure 7 : insert value
! ---------------------------
   SUBROUTINE rowCSRMatrixInsertValue(targetRowCSRMatrix,i1,val)

!     Declaration
!     - - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType, POINTER :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      CALL memoryArrayInsertValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

! Procedure 8 : add value
! ---------------------------
   SUBROUTINE rowCSRMatrixAddValue(targetRowCSRMatrix,i1,val)

!     Declaration
!     - - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType, POINTER :: val

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(arrayType), INTENT(IN) :: targetRowCSRMatrix
      CALL setWorkingArray(targetRowCSRMatrix)

!     Body
!     - - -
      CALL memoryArrayAddValue(i1,val)

!     Nullify pointer
!     - - - - - - - -
      CALL nullifyArrayPointer()

   END SUBROUTINE

END MODULE moduleRowCSRMatrix


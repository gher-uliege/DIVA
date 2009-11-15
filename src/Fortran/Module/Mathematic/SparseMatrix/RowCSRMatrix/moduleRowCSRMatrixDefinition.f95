MODULE moduleRowCSRMatrixDefinition

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

  USE moduleVectorDefinitionValue
  USE moduleVectorDefinitionIndex

! Declaration
! ===========
!  row for csrMatrix
!  -----------------
   TYPE rowCSRMatrixType
      TYPE(vectorTypeValue) :: values
      TYPE(vectorTypeIndex) :: columnIndex
   END TYPE


END MODULE moduleRowCSRMatrixDefinition


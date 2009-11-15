MODULE moduleCSRMatrixDefinition

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

! Declaration
! ===========
!  row for csrMatrix
!  -----------------
   TYPE csrMatrixType

!  1) Data related to memory management
!  ------------------------------------
!     1.1) Flag for memory allocation status
!     + + + + + + + + + + + + + + + + + + + +
       LOGICAL :: isAllocated   ! true if array memory is allocated, false if not

!  2) Array dimension
!  ------------------
      INTEGER :: nbOfRow

!  3) Values storage
!  -----------------
!     3.1) storage of the row values
!     + + + + + + + + + + + + + + + +
      TYPE(rowCSRMatrixType), DIMENSION(:), POINTER :: rows

!     3.2) pointer on the row (this pointer allows easy swapping of rows, adding rows in the matrix,...)
!     + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
      TYPE(rowCSRMatrixType), DIMENSION(:), POINTER :: rowIndex

   END TYPE

END MODULE moduleCSRMatrixDefinition


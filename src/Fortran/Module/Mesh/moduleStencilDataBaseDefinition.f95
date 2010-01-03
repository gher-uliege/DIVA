MODULE moduleStencilDataBaseDefinition

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
   USE moduleStencilDefinition

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: printInformation, initialise


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
! Procedure 1 : print Information
! --------------------------------
SUBROUTINE printInformation(output,ptr)

!     Declaration
!     - - - - - -
      INTEGER :: output
      TYPE(stencilType), POINTER :: ptr

!     Body
!     - - -
      WRITE(output,*)    'object type is stencil'
      WRITE(output,*)    '   index        = ', ptr%indexValue
      WRITE(output,*)    '   lastPosition = ', ptr%lastPosition
      CALL vectorPrint(ptr%stencil)
      WRITE(output,*)    ' '

END SUBROUTINE

! Procedure 2 : initialise
! ------------------------
SUBROUTINE initialise(ptrTarget,indexValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: indexValue
      TYPE(stencilType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%indexValue = indexValue
      ptrTarget%lastPosition = 0
      CALL vectorCreate(ptrTarget%stencil)

END SUBROUTINE

END MODULE moduleStencilDataBaseDefinition

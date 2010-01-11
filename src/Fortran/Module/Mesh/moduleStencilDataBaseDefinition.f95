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
   PUBLIC :: printInformation, initialise, destroy


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
      WRITE(output,*)    '   index            = ', ptr%indexValue
      WRITE(output,*)    '   lastPositionNode = ', ptr%lastPositionNode
      CALL vectorPrint(ptr%stencilNode)
      WRITE(output,*)    '   lastPositionCell = ', ptr%lastPositionCell
      CALL vectorPrint(ptr%stencilCell)
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
      ptrTarget%lastPositionNode = 0
      CALL vectorCreate(ptrTarget%stencilNode)

      ptrTarget%lastPositionCell = 0
      CALL vectorCreate(ptrTarget%stencilCell)

END SUBROUTINE

! Procedure 3 : destroy
! ------------------------
SUBROUTINE destroy(ptrTarget)

!     Declaration
!     - - - - - -
      TYPE(stencilType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      CALL vectorDestroy(ptrTarget%stencilCell)
      CALL vectorDestroy(ptrTarget%stencilNode)

END SUBROUTINE

END MODULE moduleStencilDataBaseDefinition

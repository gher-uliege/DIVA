MODULE moduleNodeDataBaseDefinition

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
   USE moduleNodeDefinition

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
      TYPE(nodeType), POINTER :: ptr

!     Body
!     - - -
      WRITE(output,*)    'object type is node'
      WRITE(output,*)    '   index  = ', ptr%indexValue
      WRITE(output,*)    '   xValue = ', ptr%xValue
      WRITE(output,*)    '   yValue = ', ptr%yValue
      WRITE(output,*)    '   zValue = ', ptr%zValue
      WRITE(output,*)    '   characteristicLength = ', ptr%characteristicLength
      WRITE(output,*)    ' '

END SUBROUTINE

! Procedure 2 : initialise
! ------------------------
SUBROUTINE initialise(ptrTarget,indexValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: indexValue
      TYPE(nodeType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%xValue = 0.
      ptrTarget%yValue = 0.
      ptrTarget%zValue = 0.

      ptrTarget%indexValue = indexValue
      ptrTarget%characteristicLength = 0.

END SUBROUTINE

! Procedure 3 : destroy
! ------------------------
SUBROUTINE destroy(ptrTarget)

!     Declaration
!     - - - - - -
      TYPE(nodeType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -

      ptrTarget%xValue = 0.

END SUBROUTINE

END MODULE moduleNodeDataBaseDefinition

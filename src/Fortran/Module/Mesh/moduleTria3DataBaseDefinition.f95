MODULE moduleTria3DataBaseDefinition

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
   USE moduleTria3Definition

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
      TYPE(tria3Type), POINTER :: ptr

!     Body
!     - - -
      WRITE(output,*)    'object type is tria3 cell'
      WRITE(output,*)    '   index  = ', ptr%indexValue
      WRITE(output,*)    '   node1  = ', ptr%node1
      WRITE(output,*)    '   node2  = ', ptr%node2
      WRITE(output,*)    '   node3  = ', ptr%node3
      WRITE(output,*)    '   neighbor1  = ', ptr%neighbor1
      WRITE(output,*)    '   neighbor2  = ', ptr%neighbor2
      WRITE(output,*)    '   neighbor3  = ', ptr%neighbor3
      WRITE(output,*)    ' '

END SUBROUTINE

! Procedure 2 : initialise
! ------------------------
SUBROUTINE initialise(ptrTarget,indexValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: indexValue
      TYPE(tria3Type), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%indexValue = indexValue
      ptrTarget%node1 = 0
      ptrTarget%node2 = 0
      ptrTarget%node3 = 0
      ptrTarget%neighbor1 = 0
      ptrTarget%neighbor2 = 0
      ptrTarget%neighbor3 = 0

END SUBROUTINE

! Procedure 3 : destroy
! ------------------------
SUBROUTINE destroy(ptrTarget)

!     Declaration
!     - - - - - -
      TYPE(tria3Type), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%node1 = 0

END SUBROUTINE

END MODULE moduleTria3DataBaseDefinition

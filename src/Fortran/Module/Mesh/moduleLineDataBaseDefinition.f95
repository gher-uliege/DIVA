MODULE moduleLineDataBaseDefinition

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
   USE moduleLineDefinition
   USE moduleNodeDataBaseDefinition, ONLY : printNode => printInformation

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
      TYPE(lineType), POINTER :: ptr

!     Body
!     - - -
      WRITE(output,*)    'object type is line'
      WRITE(output,*)    '   index  = ', ptr%indexValue
      CALL printNode(output,ptr%startNode)
      CALL printNode(output,ptr%endNode)
      WRITE(output,*)    ' '

END SUBROUTINE

! Procedure 2 : initialise
! ------------------------
SUBROUTINE initialise(ptrTarget,indexValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: indexValue
      TYPE(lineType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%startNode => NULL()
      ptrTarget%endNode => NULL()

      ptrTarget%indexValue = indexValue
      ptrTarget%characteristicLength = 0.

END SUBROUTINE

END MODULE moduleLineDataBaseDefinition

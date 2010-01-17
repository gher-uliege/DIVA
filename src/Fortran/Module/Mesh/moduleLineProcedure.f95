MODULE moduleLineProcedure

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

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: lineComputeLength


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
! Procedure 1 : compute the length of the line
! --------------------------------------------
FUNCTION lineComputeLength(ptr) RESULT(val)

!     Declaration
!     - - - - - -
      TYPE(lineType), POINTER :: ptr
      TYPE(nodeType), POINTER :: ptrNodeStart, ptrNodeEnd

      REAL(KIND=8) :: val, dx, dy

!     Body
!     - - -
      ptrNodeStart => ptr%startNode
      ptrNodeEnd => ptr%endNode

      dx = ptrNodeStart%xValue - ptrNodeEnd%xValue
      dy = ptrNodeStart%yValue - ptrNodeEnd%yValue

      val = sqrt( dx * dx + dy * dy )

END FUNCTION


END MODULE moduleLineProcedure

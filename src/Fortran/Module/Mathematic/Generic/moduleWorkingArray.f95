MODULE moduleWorkingArray

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
   USE moduleArrayDefinition

! Declaration
! ===========

!  General part
!  ------------
   TYPE (arrayType), PUBLIC, POINTER :: workingArray => NULL()

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: setWorkingArray, nullifyArrayPointer


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


! ============================================================
! ===            Internal procedure ("PUBLIC")             ===
! ============================================================

! Procedure 1 : setting pointer to array
! ---------------------------------------
   SUBROUTINE setWorkingArray(targetArray)

!     Declaration
!     - - - - - -
      TYPE(arrayType), INTENT(IN), TARGET :: targetArray

!     Body
!     - - -
      workingArray => targetArray

   END SUBROUTINE

! Procedure 2 : make the target of the pointer null
! --------------------------------------------------
   SUBROUTINE nullifyArrayPointer()

!     Body
!     - - -
      workingArray => NULL()

   END SUBROUTINE

END MODULE moduleWorkingArray

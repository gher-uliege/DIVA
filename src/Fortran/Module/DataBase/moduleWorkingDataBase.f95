MODULE moduleWorkingDataBase

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
   USE moduleGenericTypeDataBaseDefinition

! Declaration
! ===========

!  General part
!  ------------
   TYPE (genericTypeDataBase), PUBLIC, POINTER :: workingDataBase => NULL()

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: setWorkingDataBase, nullifyDataBasePointer


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

! Procedure 1 : setting pointer to DataBase
! ---------------------------------------
   SUBROUTINE setWorkingDataBase(targetDataBase)

!     Declaration
!     - - - - - -
      TYPE(genericTypeDataBase), INTENT(IN), TARGET :: targetDataBase

!     Body
!     - - -
      workingDataBase => targetDataBase

   END SUBROUTINE

! Procedure 2 : make the target of the pointer null
! --------------------------------------------------
   SUBROUTINE nullifyDataBasePointer()

!     Body
!     - - -
      workingDataBase => NULL()

   END SUBROUTINE

END MODULE moduleWorkingDataBase

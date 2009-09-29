MODULE moduleDIVA
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
   USE logicalUnitManager, initialiseLogicalUnitManager => initialiseDefault, deleteLogicalUnitDataBase => destructor

! Procedures status
! =================
   PUBLIC :: createDIVAContext, finaliseDIVAContext

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
! ===            External procedure ("PUBLIC")             ===
! ============================================================

! Procedure 1 : initialisation of the context DIVA
! ------------------------------------------------
   SUBROUTINE createDIVAContext()

!     Body
!     - - -
      CALL initialiseLogicalUnitManager()

   END SUBROUTINE

! Procedure 2 : finalisation of the context DIVA
! ----------------------------------------------
   SUBROUTINE finaliseDIVAContext()

!     Body
!     - - -
      CALL deleteLogicalUnitDataBase()

   END SUBROUTINE

END MODULE moduleDIVA

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
   USE logicalUnitManager, ONLY : initialiseLogicalUnitManager => initialiseDefault, deleteLogicalUnitDataBase => destructor
   USE moduleChrono, ONLY : initialiseChrono => initialise
   USE moduleNorm
   USE moduleInsertValueMethod

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
      CALL initialiseChrono()

   END SUBROUTINE

! Procedure 2 : finalisation of the context DIVA
! ----------------------------------------------
   SUBROUTINE finaliseDIVAContext()

!     Body
!     - - -
      CALL deleteLogicalUnitDataBase()

   END SUBROUTINE

END MODULE moduleDIVA

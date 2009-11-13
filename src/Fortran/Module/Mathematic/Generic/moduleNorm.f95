MODULE moduleNorm
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

! Declaration
! ===========
#ifndef _MODULE_NORM_
#define _MODULE_NORM_
   INCLUDE 'norm.h'
#endif

   TYPE(normType), PUBLIC, PARAMETER :: NORM_L1 = normType(1)
   TYPE(normType), PUBLIC, PARAMETER :: NORM_L2 = normType(2)
   TYPE(normType), PUBLIC, PARAMETER :: NORM_INF = normType(3)

! Interface
! =========
  INTERFACE operator (==)
     MODULE PROCEDURE normEqual
  END INTERFACE

! Procedures status
! =================
   PUBLIC :: getNormType
   PRIVATE :: normEqual
   
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

! ===========================================================
! ===            Internal procedure ("PUBLIC")            ===
! ===========================================================

! Procedure 1 : get the norm type
! -------------------------------
  FUNCTION getNormType(normSelection) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue
      TYPE(normType), INTENT(IN) :: normSelection

!     Body
!     - - -
      ivalue = normSelection%normTypeValue

  END FUNCTION

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================

! Procedure 1 : equality
! ----------------------
  FUNCTION normEqual(inorm1,inorm2) RESULT(check)

!     Declaration
!     - - - - - -
      LOGICAL :: check
      TYPE(normType), INTENT(IN) :: inorm1, inorm2

!     Body
!     - - -
      check = .FALSE.
      IF ( inorm1%normTypeValue == inorm2%normTypeValue ) THEN
         check = .TRUE.
      END IF

  END FUNCTION



END MODULE moduleNorm




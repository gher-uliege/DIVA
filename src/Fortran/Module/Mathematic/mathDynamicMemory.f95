MODULE mathDynamicMemory

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
   include 'ioParameter.h'
   include 'constantParameter.h'
   include 'logicalParameter.h'

! Declaration
! ===========
!  Memory part
!  -----------
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSize = 100
   INTEGER, PRIVATE :: increaseSize

! Procedures status
! =================
   PUBLIC :: initialise, mathSetMemoryIncreaseSize, memoryGetDefaultIncreaseSize

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

! Procedure 1 : initialisation
! ----------------------------
  SUBROUTINE initialise()

!     Body
!     - - -
      CALL mathSetMemoryIncreaseSize(defaultIncreaseSize)

  END SUBROUTINE

! Procedure 2 : define the extra size for allocate vector
! --------------------------------------------------------
  SUBROUTINE mathSetMemoryIncreaseSize(extraSize)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: extraSize

!     Body
!     - - -
      increaseSize = extraSize
      
  END SUBROUTINE

! Procedure 3 : get default increase size for vector
! --------------------------------------------------
  FUNCTION memoryGetDefaultIncreaseSize() RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Body
!     - - -
      dim = increaseSize

  END FUNCTION


END MODULE mathDynamicMemory


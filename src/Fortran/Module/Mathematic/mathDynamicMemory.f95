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
   INCLUDE 'ioParameter.h'
   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Declaration
! ===========
!  Memory part
!  -----------
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSize = 10
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeX = 10
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeY = 10
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeZ = 10
   INTEGER, PRIVATE :: increaseSize, increaseSizeX, increaseSizeY, increaseSizeZ

! Procedures status
! =================
   PUBLIC :: initialise, mathSetMemoryIncreaseSize, memoryGetDefaultIncreaseSize, memoryGetDefaultIncreaseSizeX, &
             memoryGetDefaultIncreaseSizeY

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
      CALL mathSetMemoryIncreaseSize(defaultIncreaseSizeX,defaultIncreaseSizeY,defaultIncreaseSizeZ)

  END SUBROUTINE

! Procedure 2 : define the extra size for allocate vector/array
! -------------------------------------------------------------
  SUBROUTINE mathSetMemoryIncreaseSize(extraSizeX, extraSizeY, extraSizeZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: extraSizeX
      INTEGER, OPTIONAL, INTENT(IN) :: extraSizeY, extraSizeZ

!     Body
!     - - -
      IF ( PRESENT(extraSizeY) ) THEN
         increaseSizeX = extraSizeX
         increaseSizeY = extraSizeY
         IF ( PRESENT(extraSizeZ) ) THEN
            increaseSizeZ = extraSizeZ
         END IF
      ELSE
         increaseSize = extraSizeX
      END IF
      
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

! Procedure 4 : get default increase size for array
! --------------------------------------------------
  FUNCTION memoryGetDefaultIncreaseSizeX() RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Body
!     - - -
      dim = increaseSizeX

  END FUNCTION

! Procedure 5 : get default increase size for array
! --------------------------------------------------
  FUNCTION memoryGetDefaultIncreaseSizeY() RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Body
!     - - -
      dim = increaseSizeY

  END FUNCTION

! Procedure 4 : get default increase size for array
! --------------------------------------------------
  FUNCTION memoryGetDefaultIncreaseSizeZ() RESULT(dim)

!     Declaration
!     - - - - - -
      INTEGER :: dim

!     Body
!     - - -
      dim = increaseSizeZ

  END FUNCTION

END MODULE mathDynamicMemory


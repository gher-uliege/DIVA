MODULE templatearrayType

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
!  Vector type
!  -----------
   TYPE arrayType
       LOGICAL :: isAllocated
       INTEGER :: nbOfDataX, nbOfDataY
       INTEGER :: allocatedSizeX, allocatedSizeY
       INTEGER :: startValueX, startValueY
       VARType, DIMENSION(:,:), POINTER :: values
   END TYPE
   
END MODULE templatearrayType


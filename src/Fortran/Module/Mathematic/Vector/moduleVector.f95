MODULE templatevectorType

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
   TYPE vectorType
       LOGICAL :: isAllocated
       INTEGER :: nbOfData
       INTEGER :: allocatedSize
       INTEGER :: startValue
       INTEGER :: increaseSize
       VARType, DIMENSION(:), POINTER :: values
   END TYPE
   
END MODULE templatevectorType


MODULE moduleContourDataBaseDefinition

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
   USE moduleContourDefinition
   USE moduleLineDataBase, ONLY : lineDBCreate, lineDBPrint

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: printInformation, initialise


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
! Procedure 1 : print Information
! --------------------------------
SUBROUTINE printInformation(output,ptr)

!     Declaration
!     - - - - - -
      INTEGER :: output
      TYPE(contourType), POINTER :: ptr

!     Body
!     - - -
      WRITE(output,*)    'object type is contour'
      WRITE(output,*)    '   index  = ', ptr%indexValue
      CALL lineDBPrint(ptr%lineDB)
      WRITE(output,*)    ' '

END SUBROUTINE

! Procedure 2 : initialise
! ------------------------
SUBROUTINE initialise(ptrTarget,indexValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: indexValue
      TYPE(contourType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      ptrTarget%indexValue = indexValue
      CALL lineDBCreate(ptrTarget%lineDB)
      CALL lineDBInitialise(ptrTarget%lineDB)

END SUBROUTINE

END MODULE moduleContourDataBaseDefinition

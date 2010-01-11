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
   PUBLIC :: printInformation, initialise, destroy


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

      WRITE(output,*)    '   the contour has ', vectorGetSize(ptr%insideContour), ' holes'
      CALL vectorPrint(ptr%insideContour)
      IF ( ptr%meshFlag ) THEN
         WRITE(output,*)    '   the domain defined by this contour has to be meshed'
      ELSE
         WRITE(output,*)    '   the domain defined by this contour does not have to be meshed'
      ENDIF
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
      CALL vectorCreate(ptrTarget%insideContour)

END SUBROUTINE

! Procedure 3 : destroy
! ------------------------
SUBROUTINE destroy(ptrTarget)

!     Declaration
!     - - - - - -
      TYPE(contourType), INTENT(INOUT) :: ptrTarget

!     Body
!     - - -
      CALL vectorDestroy(ptrTarget%insideContour)
      CALL lineDBDestroy(ptrTarget%lineDB)

END SUBROUTINE

END MODULE moduleContourDataBaseDefinition

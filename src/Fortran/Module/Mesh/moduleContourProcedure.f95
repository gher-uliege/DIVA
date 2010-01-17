MODULE moduleContourProcedure

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

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: contourComputeSurface


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
! Procedure 1 : compute the surface of the loop
! ---------------------------------------------
FUNCTION contourComputeSurface(ptr) RESULT(surface)

!     Declaration
!     - - - - - -
      TYPE(contourType), POINTER :: ptr
      TYPE(lineDataBase), POINTER :: ptrLineDB
      TYPE(lineType), POINTER :: ptrLine
      TYPE(nodeType), POINTER :: ptrNodeStart, ptrNodeEnd


      VARType :: xCenter, yCenter, surface
      VARType, DIMENSION(2) :: a, b
      INTEGER :: i1, nbOfBoundarySegment

!     Body
!     - - -

    surface = 0.
    ptrLineDB => ptr%lineDB
    nbOfBoundarySegment = lineDBGetSize(ptrLineDB)

    xCenter = sum(ptrLineDB%values(1:nbOfBoundarySegment)%startNode%xValue) / max(nbOfBoundarySegment,1)
    yCenter = sum(ptrLineDB%values(1:nbOfBoundarySegment)%startNode%yValue) / max(nbOfBoundarySegment,1)

    DO i1 = 1, nbOfBoundarySegment
       ptrLine => ptrLineDB%values(i1)
       ptrNodeStart => ptrLine%startNode
       ptrNodeEnd => ptrLine%endNode

       a(1) = ptrNodeEnd%xValue - ptrNodeStart%xValue
       a(2) = ptrNodeEnd%yValue - ptrNodeStart%yValue
       b(1) = xCenter - ptrNodeStart%xValue
       b(2) = yCenter - ptrNodeStart%yValue

       surface = surface + 0.5 * ( a(1)*b(2)-a(2)*b(1) )
    ENDDO

END FUNCTION


END MODULE moduleContourProcedure

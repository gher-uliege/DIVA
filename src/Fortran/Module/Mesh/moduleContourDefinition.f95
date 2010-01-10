MODULE moduleContourDefinition

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

USE moduleLineDataBase
USE vectorInterface

TYPE contourType

  TYPE(lineDataBase) :: lineDB
  TYPE(vectorInteger4) :: insideContour
  LOGICAL :: meshFlag

  INTEGER(KIND=4) :: indexValue

END TYPE contourType

END MODULE moduleContourDefinition

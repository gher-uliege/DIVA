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

  INTEGER :: indexValue

END TYPE contourType

END MODULE moduleContourDefinition

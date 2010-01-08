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

TYPE contourType

  TYPE(lineDataBase) :: lineDB

  INTEGER(KIND=4) :: indexValue

END TYPE contourType

END MODULE moduleContourDefinition

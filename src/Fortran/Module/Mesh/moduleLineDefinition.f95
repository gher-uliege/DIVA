MODULE moduleLineDefinition

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

USE moduleNodeDefinition

TYPE lineType

  TYPE(nodeType), POINTER :: startNode
  TYPE(nodeType), POINTER :: endNode

  INTEGER(KIND=4) :: indexValue
  VARType :: characteristicLength

END TYPE lineType

END MODULE moduleLineDefinition

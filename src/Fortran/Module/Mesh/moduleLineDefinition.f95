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

  TYPE(nodeType) :: startNode
  TYPE(nodeType) :: endNode

  INTEGER :: indexValue
  VARType :: characteristicLength

END TYPE lineType

END MODULE moduleLineDefinition

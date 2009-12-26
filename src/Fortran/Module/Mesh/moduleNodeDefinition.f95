MODULE moduleNodeDefinition

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

TYPE nodeType

  VARType :: xValue
  VARType :: yValue
  VARType :: zValue

  INTEGER(KIND=4) :: indexValue
  VARType :: characteristicLength

END TYPE nodeType

END MODULE moduleNodeDefinition

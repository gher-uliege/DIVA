MODULE moduleStencilDefinition

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

USE vectorInterface

TYPE stencilType

  INTEGER(KIND=4) :: indexValue

  TYPE(vectorInteger4) :: stencilNode
  TYPE(vectorInteger4) :: stencilCell
  INTEGER(KIND=4) :: lastPositionNode
  INTEGER(KIND=4) :: lastPositionCell

END TYPE stencilType

END MODULE moduleStencilDefinition

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

  INTEGER :: indexValue

  TYPE(vectorInteger4) :: stencilNode
  TYPE(vectorInteger4) :: stencilCell
  INTEGER :: lastPositionNode
  INTEGER :: lastPositionCell

END TYPE stencilType

END MODULE moduleStencilDefinition

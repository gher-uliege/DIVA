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

  TYPE(vectorInteger4) :: stencilNode
  TYPE(vectorInteger4) :: stencilCell
  INTEGER(KIND=4) :: indexValueNode
  INTEGER(KIND=4) :: lastPositionNode
  INTEGER(KIND=4) :: indexValueCell
  INTEGER(KIND=4) :: lastPositionCell

END TYPE stencilType

END MODULE moduleStencilDefinition

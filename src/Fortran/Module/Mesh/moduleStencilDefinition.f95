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

  TYPE(vectorInteger4) :: stencil
  INTEGER(KIND=4) :: indexValue
  INTEGER(KIND=4) :: lastPosition

END TYPE stencilType

END MODULE moduleStencilDefinition

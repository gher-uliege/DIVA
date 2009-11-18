MODULE moduleFileDefinition

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

 USE moduleFileFormatType
 USE moduleLogicalUnitDefinition

 INTEGER, PARAMETER :: maxFileLengthName = 131

TYPE file

! File type description
! =====================
! logiclaUnit = pointer to the considered logical unit
! fileName    = name of the considered file
! isLinked    = true if a logical unit is defined for the file
!             = false if not
! isOpen      = true if the logical unit has opened the stream
!             = false if not
! formatFile  = format of the file

 TYPE(logicalUnit), POINTER :: logicalUnit
 CHARACTER (LEN=maxFileLengthName) :: fileName
 LOGICAL :: isLinked
 LOGICAL :: isOpened
 TYPE(fileFormatType) :: formatFile

END TYPE file

END MODULE moduleFileDefinition

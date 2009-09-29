INTEGER, PARAMETER :: fileNameMaxLength = 131

TYPE file

! File type description
! =====================
! logiclaUnit = pointer to the considered logical unit
! fileName    = name of the considered file
! isLinked    = true if a logical unit is defined for the file
!             = false if not
! isOpen      = true if the logical unit has opened the stream
!             = false if not

 TYPE(logicalUnit), POINTER :: logicalUnit
 CHARACTER (LEN = fileNameMaxLength) :: fileName
 LOGICAL :: isLinked
 LOGICAL :: isOpened
 

END TYPE file

MODULE moduleIOBase

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

! Include file
! ============
  USE moduleFileDefinition
  USE moduleFile, ONLY : getFileUnit, getFileFormat, &
                         openFileBase => openFile, &
                         closeFileBase => closeFile


! Procedures status
! =================

!     Declaration
!     - - - - - -
      INTEGER, PRIVATE, PARAMETER :: KBLANC = 10
      TYPE(file), POINTER :: targetFile
      LOGICAL :: formType
      INTEGER :: unit

!  General part
!  ------------
   PUBLIC :: setFile, getFile, setFileFormType, getFileFormType, setLogicalUnit, getLogicalUnit, &
             openFile, closeFile


! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Getting   ===
! =============================================================

! Procedure 1 : getting the logical unit of the file
! --------------------------------------------------
  FUNCTION getLogicalUnit() RESULT(val)

!    Declaration
!    - - - - - -
     INTEGER :: val

!    Body
!    - - -
     val = unit

  END FUNCTION

! Procedure 2 : getting the form type of the file
! ------------------------------------------------
  FUNCTION getFileFormType() RESULT(val)

!    Declaration
!    - - - - - -
     LOGICAL :: val

!    Body
!    - - -
     val = formType

  END FUNCTION

! Procedure 3 : getting  the file
! -------------------------------
  FUNCTION getFile() RESULT(val)

!    Declaration
!    - - - - - -
     TYPE(file), POINTER :: val

!    Body
!    - - -
     val => targetFile

  END FUNCTION

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Setting   ===
! =============================================================

! Procedure 1 : setting the logical unit of the file
! --------------------------------------------------
  SUBROUTINE setLogicalUnit(val)

!    Declaration
!    - - - - - -
     INTEGER, INTENT(IN) :: val

!    Body
!    - - -
     unit = val

  END SUBROUTINE

! Procedure 2 : getting the form type of the file
! ------------------------------------------------
  SUBROUTINE setFileFormType(val)

!    Declaration
!    - - - - - -
     TYPE(fileFormatType), INTENT(IN) :: val

!    Body
!    - - -
     formType = val%formType

  END SUBROUTINE

! Procedure 3 : setting  the file
! -------------------------------
  SUBROUTINE setFile(val)

!    Declaration
!    - - - - - -
     TYPE(file), TARGET :: val

!    Body
!    - - -
     targetFile => val

     CALL setLogicalUnit(getFileUnit(val))
     CALL setFileFormType(getFileFormat(val))

  END SUBROUTINE

! ===========================================================
! ===            Internal procedure ("PUBLIC")  : Others  ===
! ===========================================================

! Procedure 1 : open file
! -----------------------
  SUBROUTINE openFile()

!    Body
!    - - -
     CALL openFileBase(targetFile)

  END SUBROUTINE

! Procedure 2 : close file
! -----------------------
  SUBROUTINE closeFile()

!    Body
!    - - -
     CALL closeFileBase(targetFile)

  END SUBROUTINE

END MODULE  moduleIOBase

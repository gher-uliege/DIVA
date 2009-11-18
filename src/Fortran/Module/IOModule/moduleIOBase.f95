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

  INCLUDE 'constantParameter.h'
  INCLUDE 'ioParameter.h'
  INCLUDE 'logicalParameter.h'


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
             openFile, closeFile, writeBLANK,readBLANK


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

! Procedure 2 : write KBLANC at the beginning of the file
! -------------------------------------------------------
  FUNCTION writeBLANK() RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER :: icheckError
      INTEGER :: i1

!     Body
!     - - -
      icheckError = izero

      SELECT CASE (formType)
        CASE (true)
           DO i1 = 1 , KBLANC
              WRITE(unit,*,ERR=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              WRITE(unit,ERR=99)
           END DO
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 3 : close file
! -----------------------
  SUBROUTINE closeFile()

!    Body
!    - - -
     CALL closeFileBase(targetFile)

  END SUBROUTINE

! Procedure 4 : read KBLanc at the beginning of the file
! -------------------------------------------------------
  FUNCTION readBLANK() RESULT(icheckError)

!     Declaration
!     - - - - - -
      INTEGER :: icheckError

      INTEGER :: i1

!     Body
!     - - -
      icheckError = izero

      SELECT CASE (formType)
        CASE (true)
           DO i1 = 1 , KBLANC
              READ(unit,*,ERR=99,END=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              READ(unit,ERR=99,END=99)
           END DO
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 5 : compute information to structure the file (normal field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForNormalField(totalNbOfWords,nbOfWords,numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: totalNbOfWords, nbOfWords
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = totalNbOfWords / nbOfWords
      remainingWords = totalNbOfWords - nbOfWords * numberOfFullRecord

   END SUBROUTINE

! Procedure 6 : compute information to structure the file (degenerated field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForDegenerateField(numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = izero
      remainingWords = ifour

   END SUBROUTINE


END MODULE  moduleIOBase

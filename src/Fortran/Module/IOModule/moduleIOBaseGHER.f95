MODULE moduleIOBaseGHER

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
  USE moduleIOBase, ONLY : getFileFormType

  INCLUDE 'logicalParameter.h'
  INCLUDE 'constantParameter.h'
  INCLUDE 'ioParameter.h'


! Procedures status
! =================

!     Declaration
!     - - - - - -
      INTEGER, PRIVATE, PARAMETER :: KBLANC = 10

!  General part
!  ------------
   PUBLIC :: writeBLANK,readBLANK, computeInformationForNormalField,computeInformationForDegenerateField


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


! ===========================================================
! ===            Internal procedure ("PUBLIC")  : Others  ===
! ===========================================================

! Procedure 1 : write KBLANC at the beginning of the file
! -------------------------------------------------------
  FUNCTION writeBLANK(unitToWrite) RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToWrite
      INTEGER :: icheckError
      INTEGER :: i1
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           DO i1 = 1 , KBLANC
              WRITE(unitToWrite,*,ERR=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              WRITE(unitToWrite,ERR=99)
           END DO
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 2 : read KBLanc at the beginning of the file
! -------------------------------------------------------
  FUNCTION readBLANK(unitToRead) RESULT(icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToRead
      INTEGER :: icheckError
      INTEGER :: i1
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           DO i1 = 1 , KBLANC
              READ(unitToRead,*,ERR=99,END=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              READ(unitToRead,ERR=99,END=99)
           END DO
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 3 : compute information to structure the file (normal field)
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


END MODULE moduleIOBaseGHER

MODULE moduleWrite

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
   USE moduleIOBase, ONLY : setFile, getFileFormType, getLogicalUnit, &
                            openFile, closeFile, writeBLANK, computeInformationForNormalField, &
                            computeInformationForDegenerateField

   INCLUDE 'ioParameter.h'
   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: writeVector, writeMatrix, writeArray, writeData
   PRIVATE :: defineNumberOfWords, fillinNaNWithExclusionValue

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
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1 : writeVector
! -------------------------
  SUBROUTINE writeVector(fileToWrite,entries,exclusionValue,nbOfDataI,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:), INTENT(IN) :: entries

      INTEGER :: nbOfDataJ, nbOfDataK, iprecision

!     Body
!     - - -
#ifdef _REAL4_
      iprecision = ifour
#endif
#ifdef _REAL8_
      iprecision = ieight
#endif

      nbOfDataJ = ione
      nbOfDataK = ione

      CALL setFile(fileToWrite)
      CALL openFile()
      CALL writeData(getLogicalUnit(),entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
      CALL closeFile()

  END SUBROUTINE

! Procedure 2 : writeMatrix
! -------------------------
  SUBROUTINE writeMatrix(fileToWrite,entries,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:,:), INTENT(IN) :: entries

      INTEGER :: nbOfDataK, iprecision

!     Body
!     - - -
#ifdef _REAL4_
      iprecision = ifour
#endif
#ifdef _REAL8_
      iprecision = ieight
#endif

      nbOfDataK = ione

      CALL setFile(fileToWrite)
      CALL openFile()
      CALL writeData(getLogicalUnit(),entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
      CALL closeFile()

  END SUBROUTINE

! Procedure 3 : writeArray
! -------------------------
  SUBROUTINE writeArray(fileToWrite,entries,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:,:,:), INTENT(IN) :: entries

      INTEGER :: iprecision

!     Body
!     - - -
#ifdef _REAL4_
      iprecision = ifour
#endif
#ifdef _REAL8_
      iprecision = ieight
#endif

      CALL setFile(fileToWrite)
      CALL openFile()
      CALL writeData(getLogicalUnit(),entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
      CALL closeFile()

  END SUBROUTINE

! Procedure 4 : uwritc
! --------------------
   SUBROUTINE writeData(logicalUnit,entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWordsOutput)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWordsOutput
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, INTENT(IN) :: entries(*)

      LOGICAL :: checkWritingProcedure, fileFormat
      INTEGER :: i1, i2, i3, numberOfFullRecord, remainingWords, icheckError, nbOfWords

!     Body
!     - - -

      checkWritingProcedure = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )
      IF ( PRESENT(nbOfWordsOutput) ) THEN
         nbOfWords = nbOfWordsOutput
      ELSE
         nbOfWords = iminusone
      END IF

      fileFormat = getFileFormType()

!        Define writing value if normal field or degenerated
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --
      SELECT CASE (checkWritingProcedure)
        CASE (true)
           IF ( nbOfWords <= izero ) THEN
              nbOfWords = iminusone
           END IF
           CALL defineNumberOfWords(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
           CALL fillinNaNWithExclusionValue(entries,exclusionValue,nbOfDataI*nbOfDataJ*nbOfDataK)
           CALL computeInformationForNormalField(nbOfDataI*nbOfDataJ*nbOfDataK,nbOfWords,numberOfFullRecord,remainingWords)
        CASE (false)
           CALL computeInformationForDegenerateField(numberOfFullRecord,remainingWords)
      END SELECT

!        Write "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      icheckError = writeBLANK(logicalUnit)

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         Write information on size, exclusion value,...
!        --  --  --  --  --  --  --  --  --  --  --  --  --
      SELECT CASE (fileFormat)
        CASE (true)
           WRITE(logicalUnit,*,ERR=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
        CASE (false)
           WRITE(logicalUnit,ERR=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
      END SELECT

!         Write datas
!        --  --  --  --

#ifdef _REAL4_
      IF ( iprecision /= ifour ) THEN
#endif
#ifdef _REAL8_
      IF ( iprecision /= ieight ) THEN
#endif
         GOTO 99
      END IF

      i2 = izero
      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,*,ERR=99) ( ( entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,*,ERR=99) ( ( entries(i2+i3) ) , i3 = 1 , remainingWords )
        CASE (false)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,ERR=99) ( ( entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,ERR=99) ( ( entries(i2+i3) ) , i3 = 1 , remainingWords )
      END SELECT

RETURN

      IF ( PRESENT(nbOfWordsOutput) ) THEN
          nbOfWordsOutput = nbOfWords
      END IF

      RETURN

99    CONTINUE

      WRITE(stdOutput,*) 'Data error in UWRITC, not a conform file'
      WRITE(stdOutput,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
      WRITE(stdOutput,*) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue

      IF ( PRESENT(nbOfWordsOutput) ) THEN
          nbOfWordsOutput = nbOfWords
      END IF

   END SUBROUTINE

! =============================================================
! ===            Internal procedure ("PRIVATE") : Others    ===
! =============================================================


! Procedure 2 : define the number of words to write
! -------------------------------------------------
   SUBROUTINE defineNumberOfWords(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, INTENT(INOUT) :: nbOfWords

!     Body
!     - - -
      IF ( nbOfWords <= izero ) THEN
         nbOfWords = nbOfDataI * nbOfDataJ * nbOfDataK
      END IF

   END SUBROUTINE

! Procedure 3 : replacing not-a-number value in the field with exclusion value
! -----------------------------------------------------------------------------
   SUBROUTINE fillinNaNWithExclusionValue(entries,exclusionValue,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, INTENT(IN), TARGET :: entries(*)

      VARType, POINTER :: ptr
      INTEGER :: icheck, i1

!     Body
!     - - -
     icheck = izero

      DO i1 = 1, nbOfWords
        ptr => entries(i1)

#ifdef _GFORTRAN_
        IF ( isNotANumber(ptr) ) THEN
#else
        IF ( isnan(ptr) ) THEN
#endif
           ptr = exclusionValue
           icheck = icheck + ione
        END IF
      END DO

      IF ( icheck /= izero ) THEN
         WRITE(stdOutput,*) 'WARNING: ' , icheck , ' values are not numbers'
         WRITE(stdOutput,*) 'Changing them into "exclusionValue"'
      END IF

   END SUBROUTINE

! Procedure 4 : check if value is not a number (only for GFORTRAN)
! -----------------------------------------------------------------
#ifdef _GFORTRAN_
  FUNCTION isNotANumber(value) RESULT(check)

!     Declaration
!     - - - - - -
      VARType, INTENT(IN) :: value
      LOGICAL :: check

!     Body
!     - - -
      check = false

      IF ( ( value - value ) /= 0 ) THEN
         check = true
         RETURN
      END IF

  END FUNCTION

#endif

END MODULE moduleWrite

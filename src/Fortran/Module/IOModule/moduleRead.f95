MODULE moduleRead

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
                            openFile, closeFile, readBLANK, computeInformationForNormalField, &
                            computeInformationForDegenerateField

   INCLUDE 'ioParameter.h'
   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: readData
   PRIVATE :: defineNumberOfWords, readInternalData

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

! Procedure 1 : basic ureadc procedure
! ------------------------------------
   SUBROUTINE readData(fileToRead,realEntries,exclusionValue,isMatrixRegular,nbOfDataI,nbOfWords,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file), POINTER :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfWords, nbOfDataJ, nbOfDataK
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType, DIMENSION(:), POINTER :: realEntries
      LOGICAL, INTENT(OUT) :: isMatrixRegular

      INTEGER :: iprecision
      LOGICAL :: fileFormat, checkReadingProcedure
      INTEGER :: numberOfFullRecord, remainingWords, logicalUnit, icheckError, icheckEnd

!     Body
!     - - -
      CALL setFile(fileToRead)
      logicalUnit = getLogicalUnit()
      fileFormat = getFileFormType()

!        read "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      CALL openFile()
      icheckError = readBLANK()

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         read information on size, precision
!         --  --  --  --  --  --  --  --  --  --
      CALL defineNumberOfWords(logicalUnit,fileFormat,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,&
                               exclusionValue,icheckError)

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         compute information to read the file
!         --  --  --  --  --  --  --  --  --  --
      checkReadingProcedure = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )

      SELECT CASE (checkReadingProcedure)
        CASE (true)
           CALL computeInformationForNormalField(nbOfDataI*nbOfDataJ*nbOfDataK,nbOfWords,numberOfFullRecord,remainingWords)
        CASE (false)
           CALL computeInformationForDegenerateField(numberOfFullRecord,remainingWords)
      END SELECT

#ifdef _REAL4_
      IF ( iprecision /= ifour ) THEN
#endif
#ifdef _REAL8_
      IF ( iprecision /= ieight ) THEN
#endif
         GOTO 99
      END IF

!         read datas
!         --  --  --
          CALL readInternalData(logicalUnit,fileFormat,realEntries,numberOfFullRecord,nbOfWords,&
                                remainingWords,icheckError,icheckEnd)


      IF ( ( icheckError == izero ).AND.( icheckEnd == izero ) ) THEN
      ELSE IF ( icheckError == ione ) THEN
         GOTO 99
      ELSE
         GOTO 100
      END IF

      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)
      CALL closeFile()
      RETURN

99    CONTINUE
      WRITE(stdOutput,*) 'Data error in ureadc, not a conform file'
      nbOfDataI = ione
      nbOfDataJ = ione
      nbOfDataK = ione

      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)
      CALL closeFile()
      RETURN

100   CONTINUE
      WRITE(stdOutput,*) 'Data error in UREADC, EOF reached'
      WRITE(stdOutput,*) ' number of values retrieved:', icheckEnd
      nbOfDataI = izero
      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)
      CALL closeFile()

   END SUBROUTINE

! Procedure 2 : define the number of words to read
! -------------------------------------------------
   SUBROUTINE defineNumberOfWords(logicalUnit,fileFormat,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue,&
                                  icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords, iprecision
      INTEGER, INTENT(OUT) :: icheckError
      REAL(KIND=4), INTENT(OUT) :: exclusionValue

!     Body
!     - - -
      icheckError = izero

      SELECT CASE (fileFormat)
        CASE (true)
           READ(logicalUnit,*,ERR=99,END=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
        CASE (false)
           READ(logicalUnit,ERR=99,END=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! Procedure 3 : read data
! ------------------------
  SUBROUTINE readInternalData(logicalUnit,fileFormat,realEntries,numberOfFullRecord,nbOfWords,remainingWords,&
                              icheckError,icheckEnd)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd
      VARType, DIMENSION(:), POINTER :: realEntries

!     Body
!     - - -
      icheckError = izero
      icheckEnd = izero

      ALLOCATE(realEntries(numberOfFullRecord*nbOfWords+remainingWords))

      i2 = izero
      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , numberOfFullRecord
              READ(logicalUnit,*,ERR=99,END=100) ( realEntries(i2+i3) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           READ(logicalUnit,*,ERR=99,END=100) ( realEntries(i2+i3) , i3 = 1 , remainingWords )
        CASE (false)
           DO i1 = 1 , numberOfFullRecord
              READ(logicalUnit,ERR=99,END=100) ( realEntries(i2+i3) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           READ(logicalUnit,ERR=99,END=100) ( realEntries(i2+i3) , i3 = 1 , remainingWords )
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione
      RETURN

100   CONTINUE
      icheckEnd = (i1-1)*nbOfWords+i3-1
      RETURN


   END SUBROUTINE

END MODULE moduleRead

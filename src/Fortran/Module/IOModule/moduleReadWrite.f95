MODULE moduleReadWrite
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

  USE moduleFile
  
! Include file
! ============
  INCLUDE 'constantParameter.h'
  INCLUDE 'ioParameter.h'

! Interface
! =========
  INTERFACE uwritc
       MODULE PROCEDURE writeReal4StdVector, writeReal8StdVector, writeReal4Std2DMatrix, writeReal8Std2DMatrix,  &
                        writeReal4Std3DMatrix, writeReal8Std3DMatrix, uwritcReal4, uwritcReal8
  END INTERFACE uwritc

! Procedures status
! =================
  PRIVATE :: writeReal4StdVector, writeReal8StdVector, writeReal4Std2DMatrix, writeReal8Std2DMatrix,  &
             writeReal4Std3DMatrix, writeReal8Std3DMatrix, uwritcReal4, fillinNaNWithExclusionValueReal4, defineNumberOfWords, &
             computeInformationForWritingNormalField, computeInformationForWritingDegenerateField, uwritcReal8, &
             fillinNaNWithExclusionValueReal8

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

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================


! Procedure 1 : write standard vector of real*4 in GHER format
! -------------------------------------------------------------
   SUBROUTINE writeReal4StdVector(fileToWrite,vector,exclusionValue,nbOfData,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfData
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*4, INTENT(IN) :: vector(*)
      REAL*4, INTENT(IN) :: exclusionValue

      INTEGER :: nbOfWordsToUse
      REAL*8 :: real8Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfData
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF
      
      CALL uwritcReal4(fileToWrite,real8Entries,vector,exclusionValue,ifour,nbOfData,ione,ione,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 2 : write standard vector of real*8 in GHER format
! -------------------------------------------------------------
   SUBROUTINE writeReal8StdVector(fileToWrite,vector,exclusionValue,nbOfData,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfData
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*8, INTENT(IN) :: vector(*)
      REAL*8, INTENT(IN) :: exclusionValue

      INTEGER :: nbOfWordsToUse
      REAL*4 :: real4Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfData
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal8(fileToWrite,vector,real4Entries,exclusionValue,ieight,nbOfData,ione,ione,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 3 : write standard matrix (i,j) of real*4 in GHER format
! ------------------------------------------------------------------
   SUBROUTINE writeReal4Std2DMatrix(fileToWrite,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*4, INTENT(IN) :: matrix(nbOfDataI,nbOfDataJ)
      REAL*4, INTENT(IN) :: exclusionValue

      INTEGER :: nbOfWordsToUse
      REAL*8 :: real8Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal4(fileToWrite,real8Entries,matrix,exclusionValue,ifour,nbOfDataI,nbOfDataJ,ione,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 4 : write standard matrix (i,j) of real*8 in GHER format
! ------------------------------------------------------------------
   SUBROUTINE writeReal8Std2DMatrix(fileToWrite,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*8, INTENT(IN) :: matrix(nbOfDataI,nbOfDataJ)
      REAL*8, INTENT(IN) :: exclusionValue

      INTEGER :: nbOfWordsToUse
      REAL*4 :: real4Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal8(fileToWrite,matrix,real4Entries,exclusionValue,ieight,nbOfDataI,nbOfDataJ,ione,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 5 : write standard matrix (i,j,k) of real*4 in GHER format
! --------------------------------------------------------------------
   SUBROUTINE writeReal4Std3DMatrix(fileToWrite,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*4, INTENT(IN) :: matrix(nbOfDataI,nbOfDataJ,nbOfDataK)
      REAL*4, INTENT(IN) :: exclusionValue

      INTEGER :: nbOfWordsToUse
      REAL*8 :: real8Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ * nbOfDataK
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal4(fileToWrite,real8Entries,matrix,exclusionValue,ifour,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 6 : write standard matrix (i,j,k) of real*8 in GHER format
! --------------------------------------------------------------------
   SUBROUTINE writeReal8Std3DMatrix(fileToWrite,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, OPTIONAL, INTENT(INOUT) :: nbOfWords
      REAL*8, INTENT(IN) :: matrix(nbOfDataI,nbOfDataJ,nbOfDataK)
      REAL*8, INTENT(IN) :: exclusionValue
      
      INTEGER :: nbOfWordsToUse
      REAL*4 :: real4Entries(ione)

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ * nbOfDataK
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF
      CALL uwritcReal8(fileToWrite,matrix,real4Entries,exclusionValue,ieight,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 7 : basic uwritc procedure for real*4 entries
! -------------------------------------------------------
   SUBROUTINE uwritcReal4(fileToWrite,real8Entries,real4Entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, PARAMETER :: KBLANC = 10
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL*4, INTENT(IN) :: exclusionValue
      REAL*4, INTENT(IN) :: real4Entries(*)
      REAL*8, INTENT(IN) :: real8Entries(*)

      LOGICAL :: checkWritingProcedure, fileFormat
      INTEGER :: i1, i2, i3, numberOfFullRecord, remainingWords, logicalUnit
      
!     Body
!     - - -
      checkWritingProcedure = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )
      logicalUnit = getFileUnit(fileToWrite)
      fileFormat = getFileFormat(fileToWrite)

!        Define writing value if normal field or degenerated
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --
      SELECT CASE (checkWritingProcedure)
        CASE (true)
           CALL defineNumberOfWords(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
           CALL fillinNaNWithExclusionValueReal4(real4Entries,exclusionValue,nbOfDataI*nbOfDataJ*nbOfDataK)
           CALL computeInformationForWritingNormalField(nbOfDataI*nbOfDataJ*nbOfDataK,nbOfWords,numberOfFullRecord,remainingWords)
        CASE (false)
           CALL computeInformationForWritingDegenerateField(numberOfFullRecord,remainingWords)
      END SELECT

!        Write "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      CALL openFile(fileToWrite)

      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , KBLANC
              WRITE(logicalUnit,*,ERR=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              WRITE(logicalUnit,ERR=99)
           END DO
      END SELECT

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

      IF ( iprecision /= ifour ) THEN
         GOTO 99
      END IF

      i2 = izero
      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,*,ERR=99) ( ( real4Entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,*,ERR=99) ( ( real4Entries(i2+i3) ) , i3 = 1 , remainingWords )
        CASE (false)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,ERR=99) ( ( real4Entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,ERR=99) ( ( real4Entries(i2+i3) ) , i3 = 1 , remainingWords )
      END SELECT

      CALL closeFile(fileToWrite)

      RETURN

99    CONTINUE

      WRITE(stdOutput,*) 'Data error in UWRITC, not a conform file'
      WRITE(stdOutput,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
      WRITE(stdOutput,*) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
      CALL closeFile(fileToWrite)

   END SUBROUTINE

! Procedure 8 : basic uwritc procedure for real*4 entries
! -------------------------------------------------------
   SUBROUTINE uwritcReal8(fileToWrite,real8Entries,real4Entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, PARAMETER :: KBLANC = 10
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL*8, INTENT(IN) :: exclusionValue
      REAL*4, INTENT(IN) :: real4Entries(*)
      REAL*8, INTENT(IN) :: real8Entries(*)

      LOGICAL :: checkWritingProcedure, fileFormat
      INTEGER :: i1, i2, i3, numberOfFullRecord, remainingWords, logicalUnit

!     Body
!     - - -
      checkWritingProcedure = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )
      logicalUnit = getFileUnit(fileToWrite)
      fileFormat = getFileFormat(fileToWrite)

!        Define writing value if normal field or degenerated
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --
      SELECT CASE (checkWritingProcedure)
        CASE (true)
           CALL defineNumberOfWords(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)
           CALL fillinNaNWithExclusionValueReal8(real8Entries,exclusionValue,nbOfDataI*nbOfDataJ*nbOfDataK)
           CALL computeInformationForWritingNormalField(nbOfDataI*nbOfDataJ*nbOfDataK,nbOfWords,numberOfFullRecord,remainingWords)
        CASE (false)
           CALL computeInformationForWritingDegenerateField(numberOfFullRecord,remainingWords)
      END SELECT

!        Write "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      CALL openFile(fileToWrite)

      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , KBLANC
              WRITE(logicalUnit,*,ERR=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              WRITE(logicalUnit,ERR=99)
           END DO
      END SELECT

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

      IF ( iprecision /= ieight ) THEN
         GOTO 99
      END IF

      i2 = izero
      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,*,ERR=99) ( ( real8Entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,*,ERR=99) ( ( real8Entries(i2+i3) ) , i3 = 1 , remainingWords )
        CASE (false)
           DO i1 = 1 , numberOfFullRecord
              WRITE(logicalUnit,ERR=99) ( ( real8Entries(i2+i3) ) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           WRITE(logicalUnit,ERR=99) ( ( real8Entries(i2+i3) ) , i3 = 1 , remainingWords )
      END SELECT

      CALL closeFile(fileToWrite)

      RETURN

99    CONTINUE

      WRITE(stdOutput,*) 'Data error in UWRITC, not a conform file'
      WRITE(stdOutput,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc'
      WRITE(stdOutput,*) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue
      CALL closeFile(fileToWrite)

   END SUBROUTINE

! Procedure 9 : define the number of words to write
! -------------------------------------------------
   SUBROUTINE defineNumberOfWords(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, INTENT(INOUT) :: nbOfWords
      
!     Body
!     - - -
      IF ( nbOfWords == iminusone ) THEN
         nbOfWords = nbOfDataI * nbOfDataJ * nbOfDataK
      END IF
      
   END SUBROUTINE
   
! Procedure 10 : replacing not-a-number value in the field with exclusion value
! -----------------------------------------------------------------------------
   SUBROUTINE fillinNaNWithExclusionValueReal4(real4Entries,exclusionValue,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords
      REAL*4, INTENT(IN) :: exclusionValue
      REAL*4, INTENT(IN), TARGET :: real4Entries(*)
      
      REAL*4, POINTER :: ptr
      INTEGER :: icheck, i1

!     Body
!     - - -
     icheck = izero
     
      DO i1 = 1, nbOfWords
        ptr => real4Entries(i1)
        
        IF ( isnan(ptr) ) THEN
           ptr = exclusionValue
           icheck = icheck + ione
        END IF
      END DO

      IF ( icheck /= izero ) THEN
         WRITE(stdOutput,*) 'WARNING: ' , icheck , ' values are not numbers'
         WRITE(stdOutput,*) 'Changing them into "exclusionValue"'
      END IF

   END SUBROUTINE
   
! Procedure 11 : compute information to structure the file (normal field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForWritingNormalField(totalNbOfWords,nbOfWords,numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: totalNbOfWords, nbOfWords
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = totalNbOfWords / nbOfWords
      remainingWords = totalNbOfWords - nbOfWords * numberOfFullRecord

   END SUBROUTINE

! Procedure 12 : compute information to structure the file (degenerated field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForWritingDegenerateField(numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = izero
      remainingWords = ifour

   END SUBROUTINE

! Procedure 13 : replacing not-a-number value in the field with exclusion value
! -----------------------------------------------------------------------------
   SUBROUTINE fillinNaNWithExclusionValueReal8(real8Entries,exclusionValue,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords
      REAL*8, INTENT(IN) :: exclusionValue
      REAL*8, INTENT(IN), TARGET :: real8Entries(*)

      REAL*8, POINTER :: ptr
      INTEGER :: icheck, i1

!     Body
!     - - -
     icheck = izero

      DO i1 = 1, nbOfWords
        ptr => real8Entries(i1)

        IF ( isnan(ptr) ) THEN
           ptr = exclusionValue
           icheck = icheck + ione
        END IF
      END DO

      IF ( icheck /= izero ) THEN
         WRITE(stdOutput,*) 'WARNING: ' , icheck , ' values are not numbers'
         WRITE(stdOutput,*) 'Changing them into "exclusionValue"'
      END IF

   END SUBROUTINE
   


END MODULE moduleReadWrite

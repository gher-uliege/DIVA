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

! Declaration
! ===========
  INTEGER, PRIVATE, PARAMETER :: KBLANC = 10

! Interface
! =========
  INTERFACE uwritc
       MODULE PROCEDURE writeReal4StdVector, writeReal8StdVector, writeReal4Std2DMatrix, writeReal8Std2DMatrix,  &
                        writeReal4Std3DMatrix, writeReal8Std3DMatrix
  END INTERFACE uwritc
  
  INTERFACE ureadc
       MODULE PROCEDURE readReal4StdVector, readReal8StdVector, readReal4Std2DMatrix, readReal8Std2DMatrix,  &
                        readReal4Std3DMatrix, readReal8Std3DMatrix
  END INTERFACE ureadc


! Procedures status
! =================
  PRIVATE :: writeReal4StdVector, writeReal8StdVector, writeReal4Std2DMatrix, writeReal8Std2DMatrix,  &
             writeReal4Std3DMatrix, writeReal8Std3DMatrix, uwritcReal4, fillinNaNWithExclusionValueReal4, defineNumberOfWords, &
             computeInformationForWritingNormalField, computeInformationForWritingDegenerateField, uwritcReal8, &
             fillinNaNWithExclusionValueReal8, &

             readReal4StdVector, readReal8StdVector, readReal4Std2DMatrix, readReal8Std2DMatrix, readReal4Std3DMatrix, &
             readReal8Std3DMatrix, ureadcReal4, ureadcReal8, basicUreadc, defineNumberOfWordsAndPrecision, readBLANK, &
             readExclusionValue4, readExclusionValue8, computeInformationForReadingNormalField, &
             computeInformationForReadingDegenerateField, readData4, readData8, fillEntryReal4To4, fillEntryReal4To8, &
             fillEntryReal8To4, fillEntryReal8To8, checkDimensionValue, verifMatrixDimension, fillReal8Std3DRegularMatrix, &
             fillReal8Std3DDegeneratedMatrix, fillReal4Std3DRegularMatrix, fillReal4Std3DDegeneratedMatrix, &
             fillReal8Std2DRegularMatrix, fillReal8Std2DDegeneratedMatrix, fillReal4Std2DRegularMatrix, &
             fillReal4Std2DDegeneratedMatrix, fillReal8Std1DRegularMatrix, fillReal8Std1DDegeneratedMatrix, &
             fillReal4Std1DRegularMatrix, fillReal4Std1DDegeneratedMatrix

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===             Module procedures : writing              ===
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

!     Body
!     - - -
      nbOfWordsToUse = nbOfData
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF
      
      CALL uwritcReal4(fileToWrite,vector,exclusionValue,ifour,nbOfData,ione,ione,nbOfWordsToUse)

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

!     Body
!     - - -
      nbOfWordsToUse = nbOfData
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal8(fileToWrite,vector,exclusionValue,ieight,nbOfData,ione,ione,nbOfWordsToUse)

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

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal4(fileToWrite,matrix,exclusionValue,ifour,nbOfDataI,nbOfDataJ,ione,nbOfWordsToUse)

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

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal8(fileToWrite,matrix,exclusionValue,ieight,nbOfDataI,nbOfDataJ,ione,nbOfWordsToUse)

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

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ * nbOfDataK
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF

      CALL uwritcReal4(fileToWrite,matrix,exclusionValue,ifour,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWordsToUse)

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

!     Body
!     - - -
      nbOfWordsToUse = nbOfDataI * nbOfDataJ * nbOfDataK
      IF ( PRESENT(nbOfWords) ) THEN
          nbOfWordsToUse = nbOfWords
      END IF
      CALL uwritcReal8(fileToWrite,matrix,exclusionValue,ieight,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWordsToUse)

   END SUBROUTINE

! Procedure 7 : basic uwritc procedure for real*4 entries
! -------------------------------------------------------
   SUBROUTINE uwritcReal4(fileToWrite,real4Entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, PARAMETER :: KBLANC = 10
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL*4, INTENT(IN) :: exclusionValue
      REAL*4, INTENT(IN) :: real4Entries(*)

      LOGICAL :: checkWritingProcedure, fileFormat
      INTEGER :: i1, i2, i3, numberOfFullRecord, remainingWords, logicalUnit, icheckError
      
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
      CALL writeBLANK(logicalUnit,fileFormat,icheckError)

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
   SUBROUTINE uwritcReal8(fileToWrite,real8Entries,exclusionValue,iprecision,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, PARAMETER :: KBLANC = 10
      TYPE(file), INTENT(IN) :: fileToWrite
      INTEGER, INTENT(IN) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL*8, INTENT(IN) :: exclusionValue
      REAL*8, INTENT(IN) :: real8Entries(*)

      LOGICAL :: checkWritingProcedure, fileFormat
      INTEGER :: i1, i2, i3, numberOfFullRecord, remainingWords, logicalUnit, icheckError

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
      CALL writeBLANK(logicalUnit,fileFormat,icheckError)

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
   
! Procedure 14 : write KBLanc at the beginning of the file
! -------------------------------------------------------
  SUBROUTINE writeBLANK(logicalUnit,fileFormat,icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(OUT) :: icheckError
      INTEGER :: i1

!     Body
!     - - -
      icheckError = izero

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

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===         Module procedures : reading                  ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================


! Procedure 0 : read standard vector of real*4 in GHER format
! -------------------------------------------------------------
   SUBROUTINE readReal4StdVector(fileToRead,matrix,exclusionValue,nbOfData)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfData
      REAL*4, DIMENSION(:), POINTER, INTENT(OUT) :: matrix
      REAL*4, DIMENSION(:), POINTER :: vector
      REAL*4, INTENT(OUT) :: exclusionValue

      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal4(fileToRead,vector,exclusionValue,isMatrixRegular,nbOfData)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal4Std1DRegularMatrix(matrix,vector,nbOfData)
         CASE (false)
            CALL fillReal4Std1DDegeneratedMatrix(matrix,vector,nbOfData)
      END SELECT

   END SUBROUTINE

! Procedure 1 : read standard vector of real*8 in GHER format
! -------------------------------------------------------------
   SUBROUTINE readReal8StdVector(fileToRead,matrix,exclusionValue,nbOfData)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfData
      REAL*8, DIMENSION(:), POINTER, INTENT(OUT) :: matrix
      REAL*8, DIMENSION(:), POINTER :: vector
      REAL*8 :: exclusionValue8
      REAL*4, INTENT(OUT) :: exclusionValue

      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal8(fileToRead,vector,exclusionValue8,isMatrixRegular,nbOfData)
      exclusionValue = REAL(exclusionValue8,KIND=4)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal8Std1DRegularMatrix(matrix,vector,nbOfData)
         CASE (false)
            CALL fillReal8Std1DDegeneratedMatrix(matrix,vector,nbOfData)
      END SELECT

   END SUBROUTINE

! Procedure 2 : read standard matrix (i,j) of real*4 in GHER format
! ------------------------------------------------------------------
   SUBROUTINE readReal4Std2DMatrix(fileToRead,matrix,exclusionValue,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI,nbOfDataJ
      REAL*4, DIMENSION(:,:), POINTER, INTENT(OUT) :: matrix
      REAL*4, DIMENSION(:), POINTER :: vector
      REAL*4, INTENT(OUT) :: exclusionValue

      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal4(fileToRead,vector,exclusionValue,isMatrixRegular,nbOfDataI,nbOfDataJ)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal4Std2DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ)
         CASE (false)
            CALL fillReal4Std2DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ)
      END SELECT

   END SUBROUTINE

! Procedure 3 : read standard matrix (i,j) of real*8 in GHER format
! ------------------------------------------------------------------
   SUBROUTINE readReal8Std2DMatrix(fileToRead,matrix,exclusionValue,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI,nbOfDataJ
      REAL*8, DIMENSION(:,:), POINTER, INTENT(OUT) :: matrix
      REAL*8, DIMENSION(:), POINTER :: vector
      REAL*4, INTENT(OUT) :: exclusionValue
      REAL*8 :: exclusionValue8

      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal8(fileToRead,vector,exclusionValue8,isMatrixRegular,nbOfDataI,nbOfDataJ)
      exclusionValue = REAL(exclusionValue8,KIND=4)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal8Std2DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ)
         CASE (false)
            CALL fillReal8Std2DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ)
      END SELECT

   END SUBROUTINE

! Procedure 4 : read standard matrix (i,j,k) of real*4 in GHER format
! --------------------------------------------------------------------
   SUBROUTINE readReal4Std3DMatrix(fileToRead,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*4, DIMENSION(:,:,:), POINTER, INTENT(OUT) :: matrix
      REAL*4, DIMENSION(:), POINTER :: vector
      REAL*4, INTENT(OUT) :: exclusionValue

      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal4(fileToRead,vector,exclusionValue,isMatrixRegular,nbOfDataI,nbOfDataJ,nbOfDataK)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal4Std3DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)
         CASE (false)
            CALL fillReal4Std3DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)
      END SELECT

   END SUBROUTINE

! Procedure 5 : read standard matrix (i,j,k) of real*8 in GHER format
! --------------------------------------------------------------------
   SUBROUTINE readReal8Std3DMatrix(fileToRead,matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*8, DIMENSION(:,:,:), POINTER, INTENT(OUT) :: matrix
      REAL*8, DIMENSION(:), POINTER :: vector
      REAL*4, INTENT(OUT) :: exclusionValue
      REAL*8 :: exclusionValue8
      LOGICAL :: isMatrixRegular

!     Body
!     - - -

      CALL ureadcReal8(fileToRead,vector,exclusionValue8,isMatrixRegular,nbOfDataI,nbOfDataJ,nbOfDataK)
      exclusionValue = REAL(exclusionValue8,KIND=4)

      SELECT CASE (isMatrixRegular)
         CASE (true)
            CALL fillReal8Std3DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)
         CASE (false)
            CALL fillReal8Std3DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)
      END SELECT

   END SUBROUTINE

! Procedure 6 : basic ureadc procedure for real*4 entries
! -------------------------------------------------------
   SUBROUTINE ureadcReal4(fileToRead,realEntries,exclusionValue,isMatrixRegular,nbOfDataI,nbOfWords,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfWords, nbOfDataJ, nbOfDataK
      REAL*4, INTENT(OUT) :: exclusionValue
      REAL*4, INTENT(OUT), DIMENSION(:), POINTER :: realEntries
      LOGICAL, INTENT(OUT) :: isMatrixRegular

      INTEGER :: iprecision
      REAL*4, DIMENSION(:), POINTER :: internalReal4Entries
      REAL*8, DIMENSION(:), POINTER :: internalReal8Entries
      REAL*4 :: exclusionValue4
      REAL*8 :: exclusionValue8


!     Body
!     - - -
      CALL basicUreadc(fileToRead,internalReal8Entries,internalReal4Entries,exclusionValue8,exclusionValue4,iprecision,&
                        nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

      SELECT CASE (iprecision)
         CASE (ifour)
            exclusionValue = exclusionValue4
            CALL fillEntryReal4To4(nbOfWords,internalReal4Entries,realEntries)
         CASE (ieight)
            exclusionValue = REAL(exclusionValue8,KIND=4)
            CALL fillEntryReal8To4(nbOfWords,internalReal8Entries,realEntries)
      END SELECT

      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)

   END SUBROUTINE

! Procedure 7 : basic ureadc procedure for real*8 entries
! -------------------------------------------------------
   SUBROUTINE ureadcReal8(fileToRead,realEntries,exclusionValue,isMatrixRegular,nbOfDataI,nbOfWords,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfWords, nbOfDataJ, nbOfDataK
      REAL*8, INTENT(OUT) :: exclusionValue
      REAL*8, INTENT(OUT), DIMENSION(:), POINTER :: realEntries
      LOGICAL, INTENT(OUT) :: isMatrixRegular

      INTEGER :: iprecision
      REAL*4, DIMENSION(:), POINTER :: internalReal4Entries
      REAL*8, DIMENSION(:), POINTER :: internalReal8Entries
      REAL*4 :: exclusionValue4
      REAL*8 :: exclusionValue8

!     Body
!     - - -
      CALL basicUreadc(fileToRead,internalReal8Entries,internalReal4Entries,exclusionValue8,exclusionValue4,iprecision,&
                        nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

      SELECT CASE (iprecision)
         CASE (ifour)
            exclusionValue = exclusionValue4
            CALL fillEntryReal4To8(nbOfWords,internalReal4Entries,realEntries)
         CASE (ieight)
            exclusionValue = exclusionValue8
            CALL fillEntryReal8To8(nbOfWords,internalReal8Entries,realEntries)
      END SELECT

      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)

   END SUBROUTINE

! Procedure 8 : basic ureadc procedure
! ------------------------------------
   SUBROUTINE basicUreadc(fileToRead,real8Entries,real4Entries,exclusionValue8,exclusionValue4,iprecision,&
                        nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ, nbOfDataK, iprecision
      INTEGER, INTENT(OUT) :: nbOfWords
      REAL*4, INTENT(OUT), DIMENSION(:), POINTER :: real4Entries
      REAL*8, INTENT(OUT), DIMENSION(:), POINTER :: real8Entries
      REAL*4 :: exclusionValue4
      REAL*8 :: exclusionValue8

      LOGICAL :: fileFormat, checkReadingProcedure
      INTEGER :: numberOfFullRecord, remainingWords, logicalUnit, icheckError, icheckEnd

!     Body
!     - - -
      logicalUnit = getFileUnit(fileToRead)
      fileFormat = getFileFormat(fileToRead)

!        read "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      CALL openFile(fileToRead)
      CALL readBLANK(logicalUnit,fileFormat,icheckError)

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         read information on size, precision
!         --  --  --  --  --  --  --  --  --  --
      CALL defineNumberOfWordsAndPrecision(logicalUnit,fileFormat,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,icheckError)

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         read exclusion value    (this separation in the reading procedure of the exclusion value allows possibility to define
!         --  --  --  --  --  --    real*4 or real*8 exclusion value (not use yet, currently, exclusion value is always real*4))
!      SELECT CASE (iprecision)
!         CASE (ifour)
!            CALL readExclusionValue4(logicalUnit,fileFormat,exclusionValue4,icheckError)
!         CASE (ieight)
!            CALL readExclusionValue8(logicalUnit,fileFormat,exclusionValue8,icheckError)
!      END SELECT
      CALL readExclusionValue4(logicalUnit,fileFormat,exclusionValue4,icheckError)
      exclusionValue8 = exclusionValue4

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         compute information to read the file
!         --  --  --  --  --  --  --  --  --  --
      checkReadingProcedure = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )

      SELECT CASE (checkReadingProcedure)
        CASE (true)
           CALL computeInformationForReadingNormalField(nbOfDataI*nbOfDataJ*nbOfDataK,nbOfWords,numberOfFullRecord,remainingWords)
        CASE (false)
           CALL computeInformationForReadingDegenerateField(numberOfFullRecord,remainingWords)
      END SELECT

!         read datas
!         --  --  --
      SELECT CASE (iprecision)
         CASE (ifour)
            CALL readData4(logicalUnit,fileFormat,real4Entries,numberOfFullRecord,nbOfWords,remainingWords,icheckError,icheckEnd)
         CASE (ieight)
            CALL readData8(logicalUnit,fileFormat,real8Entries,numberOfFullRecord,nbOfWords,remainingWords,icheckError,icheckEnd)
      END SELECT

      IF ( ( icheckError == izero ).AND.( icheckEnd == izero ) ) THEN
      ELSE IF ( icheckError == ione ) THEN
         GOTO 99
      ELSE
         GOTO 100
      END IF

      CALL closeFile(fileToRead)
      RETURN

99    CONTINUE
      WRITE(stdOutput,*) 'Data error in ureadc, not a conform file'
      nbOfDataI = ione
      nbOfDataJ = ione
      nbOfDataK = ione

      CALL closeFile(fileToRead)
      RETURN

100   CONTINUE
      WRITE(stdOutput,*) 'Data error in UREADC, EOF reached'
      WRITE(stdOutput,*) ' number of values retrieved:', icheckEnd
      nbOfDataI = izero
      CALL closeFile(fileToRead)

   END SUBROUTINE

! Procedure 9 : define the number of words to read
! -------------------------------------------------
   SUBROUTINE defineNumberOfWordsAndPrecision(logicalUnit,fileFormat,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords, iprecision
      INTEGER, INTENT(OUT) :: icheckError

!     Body
!     - - -
      icheckError = izero

      SELECT CASE (fileFormat)
        CASE (true)
           READ(logicalUnit,*,ERR=99,END=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords
        CASE (false)
           READ(logicalUnit,ERR=99,END=99) nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! Procedure 10 : read KBLanc at the beginning of the file
! -------------------------------------------------------
  SUBROUTINE readBLANK(logicalUnit,fileFormat,icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(OUT) :: icheckError
      
      INTEGER :: i1

!     Body
!     - - -
      icheckError = izero

      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , KBLANC
              READ(logicalUnit,*,ERR=99,END=99)
           END DO
        CASE (false)
           DO i1 = 1 , KBLANC
              READ(logicalUnit,ERR=99,END=99)
           END DO
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! Procedure 11 : read exclusion value (real*4)
! -----------------------------------
  SUBROUTINE readExclusionValue4(logicalUnit,fileFormat,exclusionValue,icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER :: nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords, iprecision
      INTEGER, INTENT(OUT) :: icheckError
      REAL*4, INTENT(OUT) :: exclusionValue

!     Body
!     - - -
      icheckError = izero
      REWIND(logicalUnit)

      CALL readBLANK(logicalUnit,fileFormat,icheckError)

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

! Procedure 11 : read exclusion value (real*4)
! -----------------------------------
  SUBROUTINE readExclusionValue8(logicalUnit,fileFormat,exclusionValue,icheckError)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER :: nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords, iprecision
      INTEGER, INTENT(OUT) :: icheckError
      REAL*8, INTENT(OUT) :: exclusionValue

!     Body
!     - - -
      icheckError = izero
      REWIND(logicalUnit)

      CALL readBLANK(logicalUnit,fileFormat,icheckError)

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

! Procedure 12 : compute information to structure the file (normal field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForReadingNormalField(totalNbOfWords,nbOfWords,numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: totalNbOfWords, nbOfWords
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = totalNbOfWords / nbOfWords
      remainingWords = totalNbOfWords - nbOfWords * numberOfFullRecord

   END SUBROUTINE

! Procedure 13 : compute information to structure the file (degenerated field)
! ---------------------------------------------------------
   SUBROUTINE computeInformationForReadingDegenerateField(numberOfFullRecord,remainingWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(OUT) :: numberOfFullRecord, remainingWords

!     Body
!     - - -
      numberOfFullRecord = izero
      remainingWords = ifour

   END SUBROUTINE

! Procedure 14 : read data (real*4)
! ------------------------
  SUBROUTINE readData4(logicalUnit,fileFormat,realEntries,numberOfFullRecord,nbOfWords,remainingWords,icheckError,icheckEnd)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd
      REAL*4, INTENT(OUT), DIMENSION(:), POINTER :: realEntries

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

! Procedure 15 : read data (real*8)
! ------------------------
  SUBROUTINE readData8(logicalUnit,fileFormat,realEntries,numberOfFullRecord,nbOfWords,remainingWords,icheckError,icheckEnd)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd
      REAL*8, INTENT(OUT), DIMENSION(:), POINTER :: realEntries

!     Body
!     - - -
      icheckError = izero
      icheckEnd = izero

      ALLOCATE(realEntries(numberOfFullRecord*nbOfWords+remainingWords))

      i2 = izero
      SELECT CASE (fileFormat)
        CASE (true)
           DO i1 = 1 , numberOfFullRecord
              READ(logicalUnit,*,END=100,ERR=99) ( realEntries(i2+i3) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           READ(logicalUnit,*,END=100,ERR=99) ( realEntries(i2+i3) , i3 = 1 , remainingWords )
        CASE (false)
           DO i1 = 1 , numberOfFullRecord
              READ(logicalUnit,END=100,ERR=99) ( realEntries(i2+i3) , i3 = 1 , nbOfWords )
              i2 = i2 + nbOfWords
           END DO
           READ(logicalUnit,END=100,ERR=99) ( realEntries(i2+i3) , i3 = 1 , remainingWords )
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione
      RETURN

100   CONTINUE
      icheckEnd = (i1-1)*nbOfWords+i3-1
      RETURN

   END SUBROUTINE

! Procedure 16 : fill data vector (real*4) with reading value (real*4)
! --------------------------------------------------------------------
  SUBROUTINE fillEntryReal4To4(nbOfWords,internalRealEntries,realEntries)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords

      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: internalRealEntries
      REAL*4, DIMENSION(:), POINTER :: realEntries

      INTEGER :: i1

!     Body
!     - - -
      ALLOCATE(realEntries(nbOfWords))

      DO i1 = 1, nbOfWords
         realEntries(i1) = internalRealEntries(i1)
      END DO

      DEALLOCATE(internalRealEntries)

  END SUBROUTINE

! Procedure 17 : fill data vector (real*8) with reading value (real*4)
! --------------------------------------------------------------------
  SUBROUTINE fillEntryReal4To8(nbOfWords,internalRealEntries,realEntries)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords

      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: internalRealEntries
      REAL*8, DIMENSION(:), POINTER :: realEntries

      INTEGER :: i1

!     Body
!     - - -
      ALLOCATE(realEntries(nbOfWords))

      DO i1 = 1, nbOfWords
         realEntries(i1) = internalRealEntries(i1)
      END DO

      DEALLOCATE(internalRealEntries)

  END SUBROUTINE

! Procedure 18 : fill data vector (real*4) with reading value (real*8)
! --------------------------------------------------------------------
  SUBROUTINE fillEntryReal8To4(nbOfWords,internalRealEntries,realEntries)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords

      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: internalRealEntries
      REAL*4, DIMENSION(:), POINTER :: realEntries

      INTEGER :: i1

!     Body
!     - - -
      ALLOCATE(realEntries(nbOfWords))

      DO i1 = 1, nbOfWords
         realEntries(i1) = REAL(internalRealEntries(i1),KIND=4)
      END DO

      DEALLOCATE(internalRealEntries)

  END SUBROUTINE

! Procedure 19 : fill data vector (real*8) with reading value (real*8)
! --------------------------------------------------------------------
  SUBROUTINE fillEntryReal8To8(nbOfWords,internalRealEntries,realEntries)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfWords

      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: internalRealEntries
      REAL*8, DIMENSION(:), POINTER :: realEntries

      INTEGER :: i1

!     Body
!     - - -
      ALLOCATE(realEntries(nbOfWords))

      DO i1 = 1, nbOfWords
         realEntries(i1) = internalRealEntries(i1)
      END DO

      DEALLOCATE(internalRealEntries)

  END SUBROUTINE

! Procedure 20 : check if matrix dimensions are correct
! -----------------------------------------------------
  SUBROUTINE checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK,isMatrixRegular)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(INOUT) :: nbOfDataI, nbOfDataJ, nbOfDataK

      LOGICAL, INTENT(OUT) :: isMatrixRegular

!     Body
!     - - -
      isMatrixRegular = ( nbOfDataI > izero ).AND.( nbOfDataJ > izero ) .AND. ( nbOfDataK > izero )
      CALL verifMatrixDimension(nbOfDataI)
      CALL verifMatrixDimension(nbOfDataJ)
      CALL verifMatrixDimension(nbOfDataK)

  END SUBROUTINE

! Procedure 21 : change matrix dimensions if needed
! -------------------------------------------------
  SUBROUTINE verifMatrixDimension(size)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(INOUT) :: size

!     Body
!     - - -
      IF ( size < izero ) THEN
         size = iminusone * size
      END IF

  END SUBROUTINE

! Procedure 22 : fill 3D real*8 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal8Std3DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, i4
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*8, DIMENSION(:,:,:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ,nbOfDataK))

      i4 = 1

      DO i3 = 1 , nbOfDataK
       DO i2 = 1 , nbOfDataJ
        DO i1 = 1 , nbOfDataI
         matrix(i1,i2,i3) = vector(i4)
         i4 = i4 + 1
        END DO
       END DO
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 23 : fill 3D real*8 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal8Std3DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*8, DIMENSION(:,:,:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*8 :: initialCoordinate, DX, DY, DZ

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)
      DY = vector(ithree)
      DZ = vector(ifour)

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ,nbOfDataK))

      DO i3 = 0 , nbOfDataK - 1
       DO i2 = 0 , nbOfDataJ - 1
        DO i1 = 0 , nbOfDataI - 1
         matrix(i1+1,i2+1,i3+1) = initialCoordinate + i1 * DX + i2 * DY + i3 * DZ
        END DO
       END DO
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE

! Procedure 24 : fill 3D real*4 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal4Std3DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, i4
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*4, DIMENSION(:,:,:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ,nbOfDataK))

      i4 = 1

      DO i3 = 1 , nbOfDataK
       DO i2 = 1 , nbOfDataJ
        DO i1 = 1 , nbOfDataI
         matrix(i1,i2,i3) = vector(i4)
         i4 = i4 + 1
        END DO
       END DO
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 25 : fill 3D real*4 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal4Std3DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK
      REAL*4, DIMENSION(:,:,:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*4 :: initialCoordinate, DX, DY, DZ

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)
      DY = vector(ithree)
      DZ = vector(ifour)

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ,nbOfDataK))

      DO i3 = 0 , nbOfDataK - 1
       DO i2 = 0 , nbOfDataJ - 1
        DO i1 = 0 , nbOfDataI - 1
         matrix(i1+1,i2+1,i3+1) = initialCoordinate + i1 * DX + i2 * DY + i3 * DZ
        END DO
       END DO
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE

! Procedure 26 : fill 2D real*8 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal8Std2DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i4
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      REAL*8, DIMENSION(:,:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ))

      i4 = 1

      DO i2 = 1 , nbOfDataJ
       DO i1 = 1 , nbOfDataI
         matrix(i1,i2) = vector(i4)
         i4 = i4 + 1
       END DO
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 27 : fill 2D real*8 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal8Std2DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      REAL*8, DIMENSION(:,:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*8 :: initialCoordinate, DX, DY

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)
      DY = vector(ithree)

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ))

      DO i2 = 0 , nbOfDataJ - 1
       DO i1 = 0 , nbOfDataI - 1
        matrix(i1+1,i2+1) = initialCoordinate + i1 * DX + i2 * DY
       END DO
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE

! Procedure 28 : fill 2D real*4 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal4Std2DRegularMatrix(matrix,vector,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i4
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      REAL*4, DIMENSION(:,:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ))

      i4 = 1

      DO i2 = 1 , nbOfDataJ
       DO i1 = 1 , nbOfDataI
         matrix(i1,i2) = vector(i4)
         i4 = i4 + 1
       END DO
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 29 : fill 2D real*4 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal4Std2DDegeneratedMatrix(matrix,vector,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ
      REAL*4, DIMENSION(:,:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*4 :: initialCoordinate, DX, DY

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)
      DY = vector(ithree)

      ALLOCATE(matrix(nbOfDataI,nbOfDataJ))

      DO i2 = 0 , nbOfDataJ - 1
       DO i1 = 0 , nbOfDataI - 1
        matrix(i1+1,i2+1) = initialCoordinate + i1 * DX + i2 * DY
       END DO
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE

! Procedure 30 : fill 1D real*8 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal8Std1DRegularMatrix(matrix,vector,nbOfDataI)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i4
      INTEGER, INTENT(IN) :: nbOfDataI
      REAL*8, DIMENSION(:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI))

      i4 = 1

      DO i1 = 1 , nbOfDataI
        matrix(i1) = vector(i4)
        i4 = i4 + 1
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 31 : fill 2D real*8 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal8Std1DDegeneratedMatrix(matrix,vector,nbOfDataI)

!     Declaration
!     - - - - - -
      INTEGER :: i1
      INTEGER, INTENT(IN) :: nbOfDataI
      REAL*8, DIMENSION(:), POINTER :: matrix
      REAL*8, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*8 :: initialCoordinate, DX

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)

      ALLOCATE(matrix(nbOfDataI))

      DO i1 = 0 , nbOfDataI - 1
       matrix(i1+1) = initialCoordinate + i1 * DX
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE

! Procedure 32 : fill 1D real*4 regular matrix
! --------------------------------------------
  SUBROUTINE fillReal4Std1DRegularMatrix(matrix,vector,nbOfDataI)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i4
      INTEGER, INTENT(IN) :: nbOfDataI
      REAL*4, DIMENSION(:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector

!     Body
!     - - -

      ALLOCATE(matrix(nbOfDataI))

      i4 = 1

      DO i1 = 1 , nbOfDataI
        matrix(i1) = vector(i4)
        i4 = i4 + 1
      END DO

      DEALLOCATE(vector)
  END SUBROUTINE

 ! Procedure 33 : fill 2D real*4 degenerated matrix
! --------------------------------------------------
  SUBROUTINE fillReal4Std1DDegeneratedMatrix(matrix,vector,nbOfDataI)

!     Declaration
!     - - - - - -
      INTEGER :: i1
      INTEGER, INTENT(IN) :: nbOfDataI
      REAL*4, DIMENSION(:), POINTER :: matrix
      REAL*4, DIMENSION(:), INTENT(INOUT), POINTER :: vector
      REAL*4 :: initialCoordinate, DX

!     Body
!     - - -
      initialCoordinate = vector(ione)
      DX = vector(itwo)

      ALLOCATE(matrix(nbOfDataI))

      DO i1 = 0 , nbOfDataI - 1
       matrix(i1+1) = initialCoordinate + i1 * DX
      END DO

      DEALLOCATE(vector)

  END SUBROUTINE


END MODULE moduleReadWrite

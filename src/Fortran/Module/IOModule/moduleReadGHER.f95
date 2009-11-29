MODULE moduleReadGHER

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
                            openFile, closeFile
   USE moduleIOBaseGHER, ONLY : readBLANK, computeInformationForNormalField, &
                                computeInformationForDegenerateField

   INCLUDE 'ioParameter.h'
   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: readDataOldFormat, readVector, readMatrix, readArray, getInformationToRead
   PRIVATE :: readData, defineNumberOfWords, readInternalData, checkDimensionValue, verifMatrixDimension, readNormalField, &
              readInternalDataBis, readDegeneratedField, fillDimension

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
  SUBROUTINE readVector(fileToRead,entries,exclusionValue,nbOfDataI)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType, DIMENSION(:), POINTER :: entries

      VARType, DIMENSION(ione) :: oldEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

      CALL setFile(fileToRead)
      CALL openFile()
      CALL readData(getLogicalUnit(),entries,realMatrixEntries,realArrayEntries,oldEntries, &
                    exclusionValue,nbOfDataI,arrayType=itwo)
      CALL closeFile()

  END SUBROUTINE

! Procedure 2 : writeMatrix
! -------------------------
  SUBROUTINE readMatrix(fileToRead,entries,exclusionValue,nbOfDataI,nbOfDataJ)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType, DIMENSION(:,:), POINTER :: entries

      VARType, DIMENSION(ione) :: oldEntries
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

      CALL setFile(fileToRead)
      CALL openFile()
      CALL readData(getLogicalUnit(),realVectorEntries,entries,realArrayEntries,oldEntries, &
                    exclusionValue,nbOfDataI,nbOfDataJ,arrayType=ithree)
      CALL closeFile()

  END SUBROUTINE

! Procedure 3 : writeArray
! -------------------------
  SUBROUTINE readArray(fileToRead,entries,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ, nbOfDataK
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType, DIMENSION(:,:,:), POINTER :: entries

      VARType, DIMENSION(ione) :: oldEntries
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries

      CALL setFile(fileToRead)
      CALL openFile()
      CALL readData(getLogicalUnit(),realVectorEntries,realMatrixEntries,entries,oldEntries, &
                    exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,arrayType=ifour)
      CALL closeFile()

  END SUBROUTINE

! Procedure 4 : basic ureadc procedure
! ------------------------------------
   SUBROUTINE readData(logicalUnit,realVectorEntries,realMatrixEntries,realArrayEntries,oldEntries,exclusionValue,&
                       nbOfDataIinput,nbOfDataJinput,nbOfDataKinput,arrayType)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, OPTIONAL, INTENT(IN) :: arrayType
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfDataIinput, nbOfDataJinput, nbOfDataKinput
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType :: oldEntries(*)
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

      INTEGER :: iprecision, nbOfWords
      INTEGER :: nbOfDataI, nbOfDataJ, nbOfDataK
      LOGICAL :: fileFormat, checkReadingProcedure
      INTEGER :: numberOfFullRecord, remainingWords, icheckError, icheckEnd

      INTEGER :: checkArrayType

      checkArrayType = ione
      IF ( PRESENT(arrayType) ) THEN
         checkArrayType = arrayType
      END IF

!     Body
!     - - -

!        read "KBLANC" in the beginning of the file (user could use them)
!        --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- -- -- --
      fileFormat = getFileFormType()

      CALL getInformationToRead(logicalUnit,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,&
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
           CALL readNormalField(checkArrayType,logicalUnit,fileFormat,numberOfFullRecord,nbOfWords,&
                remainingWords,icheckError,icheckEnd,oldEntries,realVectorEntries,realMatrixEntries,realArrayEntries, &
                nbOfDataI,nbOfDataJ,nbOfDataK,iprecision)
        CASE (false)
           CALL computeInformationForDegenerateField(numberOfFullRecord,remainingWords)
           CALL readDegeneratedField(checkArrayType,logicalUnit,fileFormat,numberOfFullRecord,nbOfWords,&
                remainingWords,icheckError,icheckEnd,oldEntries,realVectorEntries,realMatrixEntries,realArrayEntries, &
                nbOfDataI,nbOfDataJ,nbOfDataK,iprecision)
      END SELECT


      IF ( ( icheckError == izero ).AND.( icheckEnd == izero ) ) THEN
      ELSE IF ( icheckError == ione ) THEN
         GOTO 99
      ELSE
         GOTO 100
      END IF

      CALL closeFile()

      CALL fillDimension(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)

      RETURN

99    CONTINUE
      WRITE(stdOutput,*) 'Data error in ureadc, not a conform file'
      nbOfDataI = ione
      nbOfDataJ = ione
      nbOfDataK = ione

      CALL closeFile()
      CALL fillDimension(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)
      RETURN

100   CONTINUE
      WRITE(stdOutput,*) 'Data error in UREADC, EOF reached'
      WRITE(stdOutput,*) ' number of values retrieved:', icheckEnd
      nbOfDataI = izero
      CALL closeFile()
      CALL fillDimension(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)

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
      VARType :: realEntries(*)

!     Body
!     - - -
      icheckError = izero
      icheckEnd = izero

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
           READ(logicalUnit,ERR=99,END=100) ( realEntries(i2+i3), i3 = 1 , remainingWords )
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione
      RETURN

100   CONTINUE
      icheckEnd = (i1-1)*nbOfWords+i3-1
      RETURN


   END SUBROUTINE

! Procedure 4 : check if matrix dimensions are correct
! -----------------------------------------------------
  SUBROUTINE checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(INOUT) :: nbOfDataI, nbOfDataJ, nbOfDataK


!     Body
!     - - -
      CALL verifMatrixDimension(nbOfDataI)
      CALL verifMatrixDimension(nbOfDataJ)
      CALL verifMatrixDimension(nbOfDataK)

  END SUBROUTINE

! Procedure 5 : change matrix dimensions if needed
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

! Procedure 6 : read normal field
! -------------------------------
  SUBROUTINE readNormalField(checkArrayType,logicalUnit,fileFormat,numberOfFullRecord,nbOfWords,&
                remainingWords,icheckError,icheckEnd,oldEntries,realVectorEntries,realMatrixEntries,realArrayEntries, &
                nbOfDataI,nbOfDataJ,nbOfDataK,iprecision)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords, checkArrayType
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK,iprecision

      VARType :: oldEntries(*)
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

#if _REAL4_
      REAL(KIND=8), DIMENSION(:), POINTER :: realVectorEntries8
      REAL(KIND=8), DIMENSION(:,:), POINTER :: realMatrixEntries8
      REAL(KIND=8), DIMENSION(:,:,:), POINTER :: realArrayEntries8
#endif

#ifdef _REAL8_
      REAL(KIND=4), DIMENSION(:), POINTER :: realVectorEntries4
      REAL(KIND=4), DIMENSION(:,:), POINTER :: realMatrixEntries4
      REAL(KIND=4), DIMENSION(:,:,:), POINTER :: realArrayEntries4
#endif

!     Body
!     - - -
      SELECT CASE (checkArrayType)
        CASE (ione)
            CALL readInternalData(logicalUnit,fileFormat,oldEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
        CASE (itwo)
            ALLOCATE(realVectorEntries(1:nbOfDataI))
#if _REAL4_
            IF ( iprecision == ifour ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realVectorEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            ELSEIF ( iprecision == ieight ) THEN
                ALLOCATE(realVectorEntries8(1:nbOfDataI))
                CALL readInternalDataBis(logicalUnit,fileFormat,realVectorEntries8,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realVectorEntries(1:NbOfDataI) = REAL(realVectorEntries8(1:nbOfDataI),KIND=4)
                DEALLOCATE(realVectorEntries8)
            END IF
#endif

#if _REAL8_
            IF ( iprecision == ifour ) THEN
                ALLOCATE(realVectorEntries4(1:nbOfDataI))
                CALL readInternalDataBis(logicalUnit,fileFormat,realVectorEntries4,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realVectorEntries(1:NbOfDataI) = realVectorEntries4(1:nbOfDataI)
                DEALLOCATE(realVectorEntries4)
            ELSEIF ( iprecision == ieight ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realVectorEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            END IF
#endif
        CASE (ithree)
            ALLOCATE(realMatrixEntries(1:nbOfDataI,1:nbOfDataJ))
#if _REAL4_
            IF ( iprecision == ifour ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realMatrixEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            ELSEIF ( iprecision == ieight ) THEN
                ALLOCATE(realMatrixEntries8(1:nbOfDataI,1:nbOfDataJ))
                CALL readInternalDataBis(logicalUnit,fileFormat,realMatrixEntries8,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realMatrixEntries(1:nbOfDataI,1:nbOfDataJ) = REAL(realMatrixEntries8(1:nbOfDataI,1:nbOfDataJ),KIND=4)
                DEALLOCATE(realMatrixEntries8)
            END IF
#endif

#if _REAL8_
            IF ( iprecision == ifour ) THEN
                ALLOCATE(realMatrixEntries4(1:nbOfDataI,1:nbOfDataJ))
                CALL readInternalDataBis(logicalUnit,fileFormat,realMatrixEntries4,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realMatrixEntries(1:nbOfDataI,1:nbOfDataJ) = realMatrixEntries4(1:nbOfDataI,1:nbOfDataJ)
                DEALLOCATE(realMatrixEntries4)
            ELSEIF ( iprecision == ieight ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realMatrixEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            END IF
#endif
        CASE (ifour)
            ALLOCATE(realArrayEntries(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK))

#if _REAL4_
            IF ( iprecision == ifour ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realArrayEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            ELSEIF ( iprecision == ieight ) THEN
                ALLOCATE(realArrayEntries8(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK))
                CALL readInternalDataBis(logicalUnit,fileFormat,realArrayEntries8,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realArrayEntries(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) = &
                                          REAL(realArrayEntries8(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK),KIND=4)
                DEALLOCATE(realArrayEntries8)
            END IF
#endif

#if _REAL8_
            IF ( iprecision == ifour ) THEN
                ALLOCATE(realArrayEntries4(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK))
                CALL readInternalDataBis(logicalUnit,fileFormat,realArrayEntries4,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                realArrayEntries(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) = realArrayEntries4(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK)
                DEALLOCATE(realArrayEntries4)
            ELSEIF ( iprecision == ieight ) THEN
                CALL readInternalData(logicalUnit,fileFormat,realArrayEntries,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            END IF
#endif
        CASE DEFAULT
           icheckError = ione
      END SELECT

  END SUBROUTINE

! Procedure 7 : read normal field
! -------------------------------
  SUBROUTINE readDegeneratedField(checkArrayType,logicalUnit,fileFormat,numberOfFullRecord,nbOfWords,&
                remainingWords,icheckError,icheckEnd,oldEntries,realVectorEntries,realMatrixEntries,realArrayEntries, &
                nbOfDataI,nbOfDataJ,nbOfDataK,iprecision)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords, checkArrayType,iprecision
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd
      INTEGER, INTENT(INOUT) :: nbOfDataI,nbOfDataJ,nbOfDataK

      VARType, DIMENSION(ifour) :: degeneratedField

      VARType :: oldEntries(*)
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

#if _REAL4_
      REAL(KIND=8), DIMENSION(ifour) :: degeneratedField8
      degeneratedField8(:) = 0.
#endif
#if _REAL8_
      REAL(KIND=4), DIMENSION(ifour) :: degeneratedField4
      degeneratedField4(:) = 0.
#endif

!     Body
!     - - -

#if _REAL4_
            IF ( iprecision == ifour ) THEN
                CALL readInternalData(logicalUnit,fileFormat,degeneratedField,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            ELSEIF ( iprecision == ieight ) THEN
                CALL readInternalDataBis(logicalUnit,fileFormat,degeneratedField8,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                degeneratedField(:) = REAL(degeneratedField8(:),KIND=4)
            END IF
#endif
#if _REAL8_
            IF ( iprecision == ifour ) THEN
                CALL readInternalDataBis(logicalUnit,fileFormat,degeneratedField4,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
                degeneratedField(:) = degeneratedField4(:)
            ELSEIF ( iprecision == ieight ) THEN
                CALL readInternalData(logicalUnit,fileFormat,degeneratedField,numberOfFullRecord,nbOfWords,&
                                  remainingWords,icheckError,icheckEnd)
            END IF
#endif

      CALL checkDimensionValue(nbOfDataI,nbOfDataJ,nbOfDataK)

      SELECT CASE (checkArrayType)
        CASE (ione)
            IF ( iprecision == ifour ) THEN
               CALL fillValue(oldEntries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)
            ELSEIF ( iprecision == ieight ) THEN
               CALL fillValue(oldEntries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)
            END IF
        CASE (itwo)
            ALLOCATE(realVectorEntries(1:nbOfDataI))
            CALL fillValue(realVectorEntries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)
        CASE (ithree)
            ALLOCATE(realMatrixEntries(1:nbOfDataI,1:nbOfDataJ))
            CALL fillValue(realMatrixEntries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)
        CASE (ifour)
            ALLOCATE(realArrayEntries(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK))
            CALL fillValue(realArrayEntries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)
        CASE DEFAULT
           icheckError = ione
      END SELECT

  END SUBROUTINE

! Procedure 8 : fillValue
! -----------------------
  SUBROUTINE fillValue(entries,degeneratedField,nbOfDataI,nbOfDataJ,nbOfDataK)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK
      VARType, DIMENSION(ifour) :: degeneratedField
      VARType :: entries(*)

      INTEGER :: ival, i1, i2, i3
      VARType :: origin, dx, dy, dz

      origin = 0.
      dx = 0.
      dy = 0.
      dz = 0.

#ifdef _REAL4_
      origin = REAL(degeneratedField(1),KIND=4)
      IF ( nbOfDataI > ione ) THEN
         dx = REAL(degeneratedField(2),KIND=4)
      END IF
      IF ( nbOfDataJ > ione ) THEN
         dy = REAL(degeneratedField(3),KIND=4)
      END IF
      IF ( nbOfDataK > ione ) THEN
         dz = REAL(degeneratedField(4),KIND=4)
      END IF
#endif
#ifdef _REAL8_
      origin = REAL(degeneratedField(1),KIND=8)
      IF ( nbOfDataI > ione ) THEN
         dx = REAL(degeneratedField(2),KIND=8)
      END IF
      IF ( nbOfDataJ > ione ) THEN
         dy = REAL(degeneratedField(3),KIND=8)
      END IF
      IF ( nbOfDataK > ione ) THEN
         dz = REAL(degeneratedField(4),KIND=8)
      END IF
#endif

!     Body
!     - - -
      ival = ione
      DO i3 = 1, nbOfDataK
        DO i1 = 1, nbOfDataI
          DO i2 = 1, nbOfDataJ
           entries(ival) = origin + (i1-1) * dx + (i2-1) * dy + (i3-1) * dz
           ival = ival + ione
        END DO
       END DO
      END DO

  END SUBROUTINE

! Procedure 9 : fill dimension
! ----------------------------
  SUBROUTINE  fillDimension(nbOfDataI,nbOfDataJ,nbOfDataK,nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)

!     Declaration
!     - - - - - -
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfDataIinput,nbOfDataJinput,nbOfDataKinput
      INTEGER, INTENT(IN) :: nbOfDataI,nbOfDataJ,nbOfDataK

!     Body
!     - - -
      IF ( PRESENT(nbOfDataIinput) ) THEN
         nbOfDataIinput = nbOfDataI
      END IF
      IF ( PRESENT(nbOfDataJinput) ) THEN
         nbOfDataJinput = nbOfDataJ
      END IF
      IF ( PRESENT(nbOfDataKinput) ) THEN
         nbOfDataKinput = nbOfDataK
      END IF

  END SUBROUTINE

! Procedure 10 : read data
! ------------------------
  SUBROUTINE readInternalDataBis(logicalUnit,fileFormat,realEntries,numberOfFullRecord,nbOfWords,remainingWords,&
                              icheckError,icheckEnd)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat
      INTEGER, INTENT(IN) :: logicalUnit, numberOfFullRecord, remainingWords, nbOfWords
      INTEGER :: i1, i2, i3
      INTEGER, INTENT(OUT) :: icheckError,icheckEnd

#if _REAL4_
      REAL(KIND=8) :: realEntries(*)
#endif
#if _REAL8_
      REAL(KIND=4) :: realEntries(*)
#endif

!     Body
!     - - -
      icheckError = izero
      icheckEnd = izero

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
           READ(logicalUnit,ERR=99,END=100) ( realEntries(i2+i3), i3 = 1 , remainingWords )
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione
      RETURN

100   CONTINUE
      icheckEnd = (i1-1)*nbOfWords+i3-1
      RETURN


   END SUBROUTINE

! Procedure 11 : basic ureadc procedure
! ------------------------------------
   SUBROUTINE readDataOldFormat(logicalUnit,oldEntries,exclusionValue,nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, OPTIONAL, INTENT(OUT) :: nbOfDataIinput, nbOfDataJinput, nbOfDataKinput
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      VARType :: oldEntries(*)
      VARType, DIMENSION(:), POINTER :: realVectorEntries
      VARType, DIMENSION(:,:), POINTER :: realMatrixEntries
      VARType, DIMENSION(:,:,:), POINTER :: realArrayEntries

!     Body
!     - - -
      CALL readData(logicalUnit,realVectorEntries,realMatrixEntries,realArrayEntries,oldEntries,exclusionValue,&
                       nbOfDataIinput,nbOfDataJinput,nbOfDataKinput)

   END SUBROUTINE

! Procedure 12 : get information to read value
! --------------------------------------------
   SUBROUTINE getInformationToRead(logicalUnit,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,&
                               exclusionValue,icheckErrorIn)

!     Declaration
!     - - - - - - -
      INTEGER, INTENT(IN) :: logicalUnit
      INTEGER, INTENT(OUT) :: nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, OPTIONAL, INTENT(OUT) :: icheckErrorIn
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      INTEGER :: icheckError

      INTEGER, INTENT(OUT) :: iprecision, nbOfWords
      LOGICAL :: fileFormat

!     Body
!     - - -

      fileFormat = getFileFormType()
      icheckError = readBLANK(logicalUnit)

      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

!         read information on size, precision
!         --  --  --  --  --  --  --  --  --  --
      CALL defineNumberOfWords(logicalUnit,fileFormat,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,&
                               exclusionValue,icheckError)

99    CONTINUE

      IF ( PRESENT(icheckErrorIn) ) THEN
         icheckErrorIn = icheckError
      END IF

   END SUBROUTINE

END MODULE moduleReadGHER

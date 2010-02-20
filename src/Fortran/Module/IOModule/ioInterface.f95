MODULE ioInterface
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
   USE logicalUnitManager
   USE moduleFile
   USE moduleIOBase, ONLY : setFileFormType,  setFileIO => setFile, getLogicalUnitIO => getLogicalUnit, &
                            openFileIO => openFile, closeFileIO => closeFile
   USE moduleReadGHERReal4, ONLY : readVectorGHERReal4 => readVector, &
                               readMatrixGHERReal4 => readMatrix, &
                               readArrayGHERReal4 => readArray, &
                               readDataOldFormatGHERReal4 => readDataOldFormat, &
                               getInformationToReadGHER => getInformationToRead
   USE moduleReadGHERReal8, ONLY : readVectorGHERReal8 => readVector, &
                               readMatrixGHERReal8 => readMatrix, &
                               readArrayGHERReal8 => readArray, &
                               readDataOldFormatGHERReal8 => readDataOldFormat
   USE moduleWriteGHERReal4, ONLY : writeVectorGHERReal4 => writeVector, &
                                writeMatrixGHERReal4 => writeMatrix, &
                                writeArrayGHERReal4 => writeArray, &
                                writeDataGHERReal4 => writeData
   USE moduleWriteGHERReal8, ONLY : writeVectorGHERReal8 => writeVector, &
                                writeMatrixGHERReal8 => writeMatrix, &
                                writeArrayGHERReal8=> writeArray, &
                                writeDataGHERReal8 => writeData

   INCLUDE 'constantParameter.h'

! Interface
! =========
   INTERFACE readFromDisk
      MODULE PROCEDURE readVectorGHERReal4, readVectorGHERReal8, &
                       readMatrixGHERReal4, readMatrixGHERReal8, &
                       readArrayGHERReal4, readArrayGHERReal8
   END INTERFACE

   INTERFACE writeOnDisk
      MODULE PROCEDURE writeVectorGHERReal4, writeVectorGHERReal8, &
                       writeMatrixGHERReal4, writeMatrixGHERReal8, &
                       writeArrayGHERReal4, writeArrayGHERReal8
   END INTERFACE

! Procedure status
! ================
  PUBLIC :: uwritc, ureadc  ! for compatibility with previous version
  PUBLIC :: uwritcNew, ureadcNew

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

! Procedure 1 : writeData to file
! -------------------------------
  SUBROUTINE uwritc(fileUnit,entries8,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: fileUnit, iprecision, nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      REAL(KIND=8), INTENT(IN) :: entries8(*)
      REAL(KIND=4), INTENT(IN) :: entries4(*)

!     Body
!     - - -
      CALL setFileFormType(GHER)
      IF ( iprecision == ifour ) THEN
         CALL writeDataGHERReal4(fileUnit,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      ELSEIF ( iprecision == ieight ) THEN
         CALL writeDataGHERReal8(fileUnit,entries8,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      END IF

  END SUBROUTINE

! Procedure 2 : readData from file
! -------------------------------! Procedure 2 : readData from file
! -------------------------------
  SUBROUTINE ureadc(fileUnit,entries8,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: fileUnit
      INTEGER, INTENT(OUT) :: nbOfWords, iprecision, nbOfDataI, nbOfDataJ, nbOfDataK
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      REAL(KIND=8), INTENT(OUT) :: entries8(*)
      REAL(KIND=4), INTENT(OUT) :: entries4(*)

!     Body
!     - - -
      CALL setFileFormType(GHER)
      CALL getInformationToReadGHER(fileUnit,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue)
      REWIND(fileUnit)
      IF ( iprecision == ifour ) THEN
         CALL readDataOldFormatGHERReal4(fileUnit,entries4,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
      ELSEIF ( iprecision == ieight ) THEN
         CALL readDataOldFormatGHERReal8(fileUnit,entries8,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
      END IF

  END SUBROUTINE

! Procedure 3 : writeData to file (new version)
! -------------------------------
  SUBROUTINE uwritcNew(fileToWrite,entries8,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: iprecision, nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      REAL(KIND=8), INTENT(IN) :: entries8(*)
      REAL(KIND=4), INTENT(IN) :: entries4(*)
      INTEGER :: fileUnit

!     Body
!     - - -
      CALL setFileIO(fileToWrite)
      CALL openFileIO()
      fileUnit = getLogicalUnitIO()
      IF ( iprecision == ifour ) THEN
         CALL writeDataGHERReal4(fileUnit,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      ELSEIF ( iprecision == ieight ) THEN
         CALL writeDataGHERReal8(fileUnit,entries8,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      END IF
      CALL closeFileIO()

  END SUBROUTINE

! Procedure 4 : readData from file (new version)
! -------------------------------
  SUBROUTINE ureadcNew(fileToRead,entries8,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      INTEGER, INTENT(OUT) :: nbOfWords, iprecision, nbOfDataI, nbOfDataJ, nbOfDataK
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      REAL(KIND=8), INTENT(OUT) :: entries8(*)
      REAL(KIND=4), INTENT(OUT) :: entries4(*)
      INTEGER :: fileUnit

!     Body
!     - - -
      CALL setFileIO(fileToRead)
      CALL openFileIO()
      fileUnit = getLogicalUnitIO()
      CALL getInformationToReadGHER(fileUnit,nbOfDataI,nbOfDataJ,nbOfDataK,iprecision,nbOfWords,exclusionValue)
      REWIND(fileUnit)
      IF ( iprecision == ifour ) THEN
         CALL readDataOldFormatGHERReal4(fileUnit,entries4,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
      ELSEIF ( iprecision == ieight ) THEN
         CALL readDataOldFormatGHERReal8(fileUnit,entries8,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
      END IF
      CALL closeFileIO()

  END SUBROUTINE


END MODULE ioInterface

MODULE moduleWriteTHK

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
   USE moduleIOBaseTHK, ONLY : writeType, writeDimension, writePrecision

   INCLUDE 'ioParameter.h'
   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Procedures status
! =================

!  Declaration
!  -----------
#ifdef _REAL4_
   INTEGER, PARAMETER :: iprecision = ifour
#endif
#ifdef _REAL8_
   INTEGER, PARAMETER :: iprecision = ieight
#endif
#ifdef _INTEGER2_
   INTEGER, PARAMETER :: iprecision = itwo
#endif
#ifdef _INTEGER4_
   INTEGER, PARAMETER :: iprecision = ifour
#endif
#ifdef _INTEGER8_
   INTEGER, PARAMETER :: iprecision = ieight
#endif


!  General part
!  ------------
   PUBLIC :: writeVector, writeMatrix, writeArray
   PRIVATE :: writeData

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
  SUBROUTINE writeVector(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      VARType, DIMENSION(:), INTENT(IN) :: entries
      LOGICAL, OPTIONAL, INTENT(IN) :: fileOpened
      LOGICAL :: fileStatus
      INTEGER :: logicalUnit, icheckError
      INTEGER, PARAMETER :: type = itwo

!     Body
!     - - -
      fileStatus = false
      IF ( PRESENT(fileOpened) ) THEN
         fileStatus = fileOpened
      END IF

      IF ( .NOT.(fileStatus) ) THEN
         CALL setFile(fileToWrite)
         CALL openFile()
      END IF

      logicalUnit = getLogicalUnit()

      icheckError = writePrecision(logicalUnit,iprecision)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError = writeType(logicalUnit,type)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeData(logicalUnit,entries,nbOfDataX)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

99    CONTINUE

      IF ( .NOT.(fileStatus) ) THEN
         CALL closeFile()
      END IF

  END SUBROUTINE

! Procedure 2 : writeMatrix
! -------------------------
  SUBROUTINE writeMatrix(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX, &
                                             increaseSizeY,nbOfDataY,firstIndexY,lastIndexY,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      INTEGER, INTENT(IN) :: increaseSizeY,nbOfDataY,firstIndexY,lastIndexY
      VARType, DIMENSION(:,:), INTENT(IN) :: entries
      LOGICAL, OPTIONAL, INTENT(IN) :: fileOpened
      LOGICAL :: fileStatus
      INTEGER :: logicalUnit, icheckError
      INTEGER, PARAMETER :: type = ithree

!     Body
!     - - -
      fileStatus = false
      IF ( PRESENT(fileOpened) ) THEN
         fileStatus = fileOpened
      END IF

      IF ( .NOT.(fileStatus) ) THEN
         CALL setFile(fileToWrite)
         CALL openFile()
      END IF

      logicalUnit = getLogicalUnit()

      icheckError = writePrecision(logicalUnit,iprecision)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError = writeType(logicalUnit,type)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeY,nbOfDataY,firstIndexY,lastIndexY)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeData(logicalUnit,entries,nbOfDataX*nbOfDataY)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

99    CONTINUE

      IF ( .NOT.(fileStatus) ) THEN
         CALL closeFile()
      END IF

  END SUBROUTINE

! Procedure 3 : writeArray
! -------------------------
  SUBROUTINE writeArray(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX, &
                                             increaseSizeY,nbOfDataY,firstIndexY,lastIndexY, &
                                             increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      INTEGER, INTENT(IN) :: increaseSizeY,nbOfDataY,firstIndexY,lastIndexY
      INTEGER, INTENT(IN) :: increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ
      VARType, DIMENSION(:,:,:), INTENT(IN) :: entries
      LOGICAL, OPTIONAL, INTENT(IN) :: fileOpened
      LOGICAL :: fileStatus
      INTEGER :: logicalUnit, icheckError
      INTEGER, PARAMETER :: type = ifour

!     Body
!     - - -
      fileStatus = false
      IF ( PRESENT(fileOpened) ) THEN
         fileStatus = fileOpened
      END IF

      IF ( .NOT.(fileStatus) ) THEN
         CALL setFile(fileToWrite)
         CALL openFile()
      END IF

      logicalUnit = getLogicalUnit()

      icheckError = writePrecision(logicalUnit,iprecision)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError = writeType(logicalUnit,type)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeY,nbOfDataY,firstIndexY,lastIndexY)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeDimension(logicalUnit,increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

      icheckError =  writeData(logicalUnit,entries,nbOfDataX*nbOfDataY*nbOfDataZ)
      IF ( icheckError == ione ) THEN
         GOTO 99
      END IF

99    CONTINUE

      IF ( .NOT.(fileStatus) ) THEN
         CALL closeFile()
      END IF

  END SUBROUTINE

! =============================================================
! ===            Internal procedure ("PRIVATE") : Others    ===
! =============================================================


! Procedure 1 : write data
! ------------------------
  FUNCTION writeData(logicalUnit,entries,nbOfWords) RESULT(icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: logicalUnit,nbOfWords
      INTEGER :: icheckError
      LOGICAL :: formType
      INTEGER :: i1
      VARType, INTENT(IN) :: entries(*)


!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           WRITE(logicalUnit,*,ERR=99) ( ( entries(i1) ) , i1 = 1 , nbOfWords )
        CASE (false)
           WRITE(logicalUnit,ERR=99) ( ( entries(i1) ) , i1 = 1 , nbOfWords )
      END SELECT

      RETURN

99    CONTINUE

      icheckError = ione

  END FUNCTION


END MODULE moduleWriteTHK

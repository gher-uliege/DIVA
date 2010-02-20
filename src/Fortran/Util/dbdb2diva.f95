PROGRAM dbdbToDiva

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE matrixInterface

! Declaration
! ===========
   TYPE(file) :: inputFile1, inputFile2
   TYPE(file) :: outputFile1, outputFile2
   INTEGER :: nbOfGridPointX, nbOfGridPointY, inputFileUnit, outputFileUnit
   REAL(KIND=4) :: gridOriginX, gridOriginY, gridStepX, gridStepY

! ==================
! ==================
! == Main program ==
! ==================
! ==================

!  Always start the DIVA context
!  =============================
   CALL createDIVAContext()

!  Body
!  ====
!     1) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.10',formType=STD_FORMATTED)
   CALL createFile(inputFile2,'fort.11',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'fort.20',formType=STD_FORMATTED)
   CALL createFile(outputFile2,'fort.12',formType=GHER_UNFORMATTED)
   
!     2) Reading
!     ----------   
     CALL openFile(inputFile1)
     inputFileUnit = getFileUnit(inputFile1) 
     
     READ(inputFileUnit,*) gridOriginX
     READ(inputFileUnit,*) gridOriginY
     READ(inputFileUnit,*) gridStepX
     READ(inputFileUnit,*) gridStepY
     READ(inputFileUnit,*) nbOfGridPointX
     READ(inputFileUnit,*) nbOfGridPointY

     CALL closeFile(inputFile1)

!     3) Writing
!     -----------   
     CALL openFile(outputFile1)
     outputFileUnit = getFileUnit(outputFile1) 
     
     WRITE(outputFileUnit,*) gridOriginX
     WRITE(outputFileUnit,*) gridOriginY
     WRITE(outputFileUnit,*) gridStepX / 60.
     WRITE(outputFileUnit,*) gridStepY / 60.
     WRITE(outputFileUnit,*) nbOfGridPointX
     WRITE(outputFileUnit,*) nbOfGridPointY

     CALL closeFile(outputFile1)

!     4) Write topology
!     -----------------
     CALL toGHER(nbOfGridPointX,nbOfGridPointY,inputFile2,outputFile2)   
         
!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

! ============================================================5
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Program procedures                  ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS

! Procedure 1 : convert to Gnu
! ----------------------------
SUBROUTINE toGHER(nbOfGridPointX,nbOfGridPointY,inputFile,outputFile)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfGridPointX, nbOfGridPointY
      TYPE(file) :: inputFile, outputFile
      TYPE(matrixReal4) :: topology
      REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrTopology
      REAL(KIND=4), PARAMETER :: exclusionValue = 999.
      INTEGER :: inputFileUnit, i1, i2

!     Body
!     - - -

!        1) Reading input file
!        + + + + + + + + + + +
     CALL openFile(inputFile)
     inputFileUnit = getFileUnit(inputFile)
     
     READ(inputFileUnit,*) 
     READ(inputFileUnit,*) 
     READ(inputFileUnit,*)
     
     PRINT*, 'Going to read ',nbOfGridPointX,nbOfGridPointY, ' matrix' 

     CALL matrixCreate(topology,nbOfGridPointX,nbOfGridPointY)
     ptrTopology => matrixGetValues(topology)
     
     DO i2 = nbOfGridPointY,1,-1
        READ(inputFileUnit,101) (ptrTopology(i1,i2),i1=1,nbOfGridPointX)
     ENDDO

     CALL closeFile(inputFile)

101   FORMAT (10F8.2)

!        2) Writing topology
!        + + + + + + + + + + +

     CALL matrixWrite(topology,outputFile,exclusionValue)
     PRINT*, 'Finished writing binary file'
     
     CALL matrixDestroy(topology)

END SUBROUTINE
   
END PROGRAM dbdbToDiva

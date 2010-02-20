PROGRAM subsampling

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE vectorInterface

  INCLUDE 'ioParameter.h'
  
! Declaration
! ===========
  INTEGER :: nbOfSample, nbOfData, seed, nbOfColumn, i1, i2, i3, iSeed
  TYPE(vectorInteger2) :: iVector
  INTEGER(KIND=2), DIMENSION(:), POINTER :: ptrVector
  TYPE(file) :: outputFile1, outputFile2, inputFile
  INTEGER :: outputFileUnit1, outputFileUnit2, inputFileUnit

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
#ifdef _BATCH_MODE_
#undef _INTERACTIVE_MODE_
#endif

!     Read information
!     ----------------
!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the number of samples'
   READ(stdInput,*) nbOfSample
   WRITE(stdOutput,*) 'Please enter the number of data'
   READ(stdInput,*) nbOfData
   WRITE(stdOutput,*) 'Please enter the seed'
   READ(stdInput,*) seed
   WRITE(stdOutput,*) 'Please enter the number of columns'
   READ(stdInput,*) nbOfColumn

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfSample,nbOfData,seed,nbOfColumn
30 CONTINUE

#endif

   PRINT*, 'Subsampling',nbOfSample,nbOfData,seed,nbOfColumn
   
!     Initialise vector
!     -----------------
   CALL vectorCreate(iVector,nbOfSample)
   CALL vectorSetToZero(iVector)
   ptrVector => vectorGetValues(iVector)
   
!     Fill iVector
!     ------------
   i1 = 0
   i2 = nint((seed+nbOfData)*1.)  
   CALL mysrand(i2,iSeed)

99 CONTINUE
   IF ( i1 >= nbOfSample )  THEN
      GOTO 100
   ENDIF
   
   i3 = nint( ( nbOfData - 1 ) * RandF(iSeed) ) + 1
   
   IF ( ptrVector(i3) == 1 ) THEN
      GOTO 99
   ENDIF
   
   ptrVector(i3) = 1
   
   i1 = i1 + 1
   GOTO 99

100 CONTINUE

!     Computing data
!     --------------
   CALL createFile(inputFile,'fort.20',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'fort.44',formType=STD_FORMATTED)
   CALL createFile(outputFile2,'fort.45',formType=STD_FORMATTED)
   
   CALL openFile(inputFile)
   CALL openFile(outputFile1)
   CALL openFile(outputFile2)
   inputFileUnit = getFileUnit(inputFile) 
   outputFileUnit1 = getFileUnit(outputFile1) 
   outputFileUnit2 = getFileUnit(outputFile2) 

   SELECT CASE (nbOfColumn)
      CASE (3)
          CALL computeForThreeColumn(inputFileUnit,outputFileUnit1,outputFileUnit2,nbOfData,ptrVector)
      CASE (4)
          CALL computeForFourColumn(inputFileUnit,outputFileUnit1,outputFileUnit2,nbOfData,ptrVector)
   END SELECT
   
   CALL closeFile(inputFile)
   CALL closeFile(outputFile1)
   CALL closeFile(outputFile2)
    
!  Always finalise the DIVA context
!  ================================
   CALL vectorDestroy(iVector)  
   CALL finaliseDIVAContext()

! ============================================================
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

! Procedure 1 : mysrand
! ---------------------
SUBROUTINE mysrand(iValue,iSeed)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iValue
      INTEGER, INTENT(INOUT) :: iSeed
      
!     Body
!     - - -
      iSeed = iValue
     
END SUBROUTINE

! Procedure 2 : randf
! -------------------
FUNCTION RandF(iSeed) RESULT(value) 
      
!     Declaration
!     - - - - - -
      INTEGER, INTENT(INOUT) :: iSeed
      
      REAL(KIND=4) :: value
      
      INTEGER, PARAMETER :: ia = 16807, ic = 2147483647, iq = 127773, ir = 2836
      INTEGER :: ih, il, it
      
!     Body
!     - - -
      ih = iSeed / iq
      il = MOD(iSeed,iq)
      it = ia * il - ir * ih

      IF ( it > 0 ) THEN
         iSeed = it
      ELSE
         iSeed = ic + it
      ENDIF      
      
      value = iSeed / FLOAT(ic)

END FUNCTION

! Procedure 3 : compute data based on three column data file
! -----------------------------------------------------------
 SUBROUTINE computeForThreeColumn(inputFileUnit,outputFileUnit1,outputFileUnit2,nbOfData,ptrVector)
 
!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit, outputFileUnit1, outputFileUnit2, nbOfData
      INTEGER(KIND=2), DIMENSION(:), POINTER :: ptrVector
      REALType :: xValue, yValue, dataValue
      INTEGER :: i1

!     Body
!     - - -
      i1 = 0

101   CONTINUE

      READ(inputFileUnit,*,END=200,ERR=200) xValue, yValue, dataValue

      i1 = i1 + 1

      IF ( i1 > nbOfData ) THEN
         i1 = nbOfData
         PRINT*, 'Problem with data file (last line?)'
         GOTO 200
      ENDIF

      IF ( ptrVector(i1) == 0 ) THEN
         WRITE(outputFileUnit1,*) xValue, yValue, dataValue 
      ELSE
         WRITE(outputFileUnit2,*) xValue, yValue, dataValue 
      ENDIF

      GOTO 101

200   CONTINUE            

END SUBROUTINE
   
! Procedure 4 : compute data based on four column data file
! -----------------------------------------------------------
 SUBROUTINE computeForFourColumn(inputFileUnit,outputFileUnit1,outputFileUnit2,nbOfData,ptrVector)
 
!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit, outputFileUnit1, outputFileUnit2, nbOfData
      INTEGER(KIND=2), DIMENSION(:), POINTER :: ptrVector
      REALType :: xValue, yValue, dataValue, weight
      INTEGER :: i1

!     Body
!     - - -
      i1 = 0

101   CONTINUE

      READ(inputFileUnit,*,END=200,ERR=200) xValue, yValue, dataValue, weight

      i1 = i1 + 1

      IF ( i1 > nbOfData ) THEN
         i1 = nbOfData
         PRINT*, 'Problem with data file (last line?)'
         GOTO 200
      ENDIF

      IF ( ptrVector(i1) == 0 ) THEN
         WRITE(outputFileUnit1,76) xValue, yValue, dataValue, weight 
      ELSE
         WRITE(outputFileUnit2,76) xValue, yValue, dataValue, weight 
      ENDIF

      GOTO 101

200   CONTINUE            
76    FORMAT(4(E19.7))

END SUBROUTINE

END PROGRAM subsampling

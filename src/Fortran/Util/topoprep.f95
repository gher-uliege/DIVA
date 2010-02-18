PROGRAM topoprep

! Module
! ======
  USE moduleDIVA
  USE moduleFile

! Include file
! ============
   INCLUDE 'constantParameter.h'
   INCLUDE 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: inputfileUnit, outputfileUnit1, outputfileUnit2, outputfileUnit3
   INTEGER :: changeCoordinate, iSpec, iReg, nbOfGridPointX, nbOfGridPointY, ii
   REALType :: lengthScale, gridOriginX, gridOriginY, gridStepX, gridStepY, exclusionValue, signalToNoiseRatio
   TYPE(file) :: inputFile, outputFile1, outputFile2, outputFile3

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

!     Read information
!     ----------------
   CALL createFile(inputFile,'fort.10',formType=STD_FORMATTED)
   CALL openFile(inputFile)
   inputfileUnit = getFileUnit(inputFile)

   READ(inputfileUnit,*)
   READ(inputfileUnit,*) lengthScale
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) changeCoordinate
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) iSpec
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) iReg
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) gridOriginX
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) gridOriginY
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) gridStepX
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) gridStepY
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) nbOfGridPointX
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) nbOfGridPointY
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) exclusionValue
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) signalToNoiseRatio
   READ(inputfileUnit,*)
   READ(inputfileUnit,*) 
   
   CALL closeFile(inputFile)

!     Write information (1)
!     -----------------
   CALL createFile(outputFile1,'fort.11',formType=STD_FORMATTED)
   CALL openFile(outputFile1)
   outputfileUnit1 = getFileUnit(outputFile1)
   
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) lengthScale
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) changeCoordinate
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) iSpec
   WRITE(outputFileUnit1,*)
   iReg = 0
   WRITE(outputFileUnit1,*) iReg
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) gridOriginX
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) gridOriginY
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) gridStepX
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) gridStepY
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) nbOfGridPointX
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) nbOfGridPointY
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) exclusionValue
   WRITE(outputFileUnit1,*)
   WRITE(outputFileUnit1,*) signalToNoiseRatio
   WRITE(outputFileUnit1,*)
   ii = 0
   WRITE(outputFileUnit1,*) ii
   
   CALL closeFile(outputFile1)

!     Write information (2)
!     -----------------
   CALL createFile(outputFile2,'fort.20',formType=STD_FORMATTED)
   CALL openFile(outputFile2)
   outputfileUnit2 = getFileUnit(outputFile2)
   
   WRITE(outputfileUnit2,*) gridOriginX
   WRITE(outputfileUnit2,*) gridOriginY
   WRITE(outputfileUnit2,*) gridStepX
   WRITE(outputfileUnit2,*) gridStepY
   WRITE(outputfileUnit2,*) nbOfGridPointX
   WRITE(outputfileUnit2,*) nbOfGridPointY
   
   CALL closeFile(outputFile2)

!     Write information (3)
!     -----------------
   CALL createFile(outputFile3,'fort.30',formType=STD_FORMATTED)
   CALL openFile(outputFile3)
   outputfileUnit3 = getFileUnit(outputFile3)
   
   ii = 1   
   WRITE(outputfileUnit3,*) ii
   ii = 4
   WRITE(outputfileUnit3,*) ii
   WRITE(outputfileUnit3,*) gridOriginX - gridStepX, gridOriginY - gridStepY
   WRITE(outputfileUnit3,*) gridOriginX + nbOfGridPointX * gridStepX, gridOriginY - gridStepY
   WRITE(outputfileUnit3,*) gridOriginX + nbOfGridPointX * gridStepX, gridOriginY + nbOfGridPointY * gridStepY
   WRITE(outputfileUnit3,*) gridOriginX - gridStepX, gridOriginY + nbOfGridPointY * gridStepY
   WRITE(outputfileUnit3,*)
   
   CALL closeFile(outputFile3)

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM  topoprep
      
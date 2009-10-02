PROGRAM cverror

! Module
! ======
  USE moduleDIVA
  USE moduleFile

! Include file
! ============
   include 'constantParameter.h'
   include 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: inputFileUnit1, inputFileUnit2, inputFileUnit3, inputFileUnit4, outputFileUnit, numberOfData
   REALType    :: exclusionValue, crossValidationValue, errorValue, errorValueBis, trace, variableData

   Type(file) :: outputFile, inputFile1, inputFile2, inputFile3, inputFile4

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
   WRITE(stdOutput,*) 'Please enter the exclusion value'
   READ(stdInput,*) exclusionValue
   WRITE(stdOutput,*) 'Please enter the cross validation  value'
   READ(stdInput,*) crossValidationValue

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) exclusionValue, crossValidationValue
30 CONTINUE

#endif

!    Opening file to read and to write
!    ---------------------------------
   CALL createFile(inputFile1,'fort.20',getLogicalUnit())
   CALL openFile(inputFile1)
   inputFileUnit1 = getFileUnit(inputFile1)

   CALL createFile(inputFile2,'fort.21',getLogicalUnit())
   CALL openFile(inputFile2)
   inputFileUnit2 = getFileUnit(inputFile2)

   CALL createFile(inputFile3,'fort.22',getLogicalUnit())
   CALL openFile(inputFile3)
   inputFileUnit3 = getFileUnit(inputFile3)

   CALL createFile(inputFile4,'fort.33',getLogicalUnit())
   CALL openFile(inputFile4)
   inputFileUnit4 = getFileUnit(inputFile4)

   CALL createFile(outputFile,'fort.23',getLogicalUnit())
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------

   CALL readVariableData(inputFileUnit4,variableData)
   CALL computeError(inputFileUnit1,inputFileUnit2,inputFileUnit3,exclusionValue,numberOfData,errorValue,errorValueBis,trace)
   CALL writeOutputFile(outputFileUnit,numberOfData,crossValidationValue,errorValue,variableData,trace,errorValueBis)

   CALL closeFile(inputFile1)
   CALL closeFile(inputFile2)
   CALL closeFile(inputFile3)
   CALL closeFile(inputFile4)
   CALL closeFile(outputFile)

!  Always finalise the DIVA context
!  ================================
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

! Procedure 1 : compute error analysis
! ------------------------------------
 SUBROUTINE computeError(inputFileUnit1,inputFileUnit2,inputFileUnit3,exclusionValue,numberOfData,errorValue,errorValueBis,trace)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit1, inputFileUnit2, inputFileUnit3
      INTEGER, INTENT(OUT) :: numberOfData

      REALType, INTENT(IN)    :: exclusionValue
      REALType, INTENT(OUT)   :: errorValue, errorValueBis, trace
      REALType :: xValue, yValue, dataValue1, dataValue2, dataValue3, diagElementAii
      REALType, PARAMETER :: tolerance = 1.D-4
!     Body
!     - - -
      numberOfData = izero
      errorValue = zero
      errorValueBis = zero
      trace = zero

101 CONTINUE
       READ(inputFileUnit1,*,END=200,ERR=200) xValue,yValue,dataValue1
       READ(inputFileUnit2,*,END=200,ERR=200) xValue,yValue,dataValue2
       READ(inputFileUnit3,*,END=200,ERR=200) xValue,yValue,dataValue3,diagElementAii

       IF ( abs(dataValue1-exclusionValue) < tolerance * abs(exclusionValue) ) GOTO 101
       IF ( abs(dataValue2-exclusionValue) < tolerance * abs(exclusionValue) ) GOTO 101

       errorValue = errorValue + ( dataValue1 - dataValue2 )**two / ( one - diagElementAii )**two
       errorValueBis = errorValueBis + ( dataValue1 - dataValue2 )**two
       trace = trace + diagElementAii
       numberOfData = numberOfData + ione
       GOTO 101

200 CONTINUE

       trace = trace / ( one * numberOfData )

 END SUBROUTINE

! Procedure 2 : write output file
! -------------------------------
 SUBROUTINE writeOutputFile(outputFileUnit,numberOfData,crossValidationValue, errorValue, variableData, trace, errorValueBis)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: outputFileUnit, numberOfData
      REALType, INTENT(IN)    :: crossValidationValue, errorValue, variableData, trace, errorValueBis

!     Body
!     - - -
       WRITE(outputFileUnit,999) crossValidationValue, sqrt(errorValue/(one*numberOfData)), variableData, trace, &
                                 sqrt(errorValueBis/(one*numberOfData)),zero,numberOfData*one
 999  format(7(E11.4))

 END SUBROUTINE

 ! Procedure 3 : read variable data
 ! --------------------------------
  SUBROUTINE readVariableData(inputFileUnit,variableData)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit
      REALType, INTENT(OUT)   :: variableData

!     Body
!     - - -
      READ(inputFileUnit,*) variableData

  END SUBROUTINE

END PROGRAM cverror

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
   INTEGERType :: inputFileUnit1, inputFileUnit2, outputFileUnit, numberOfData
   REALType    :: exclusionValue, errorValue

   Type(file) :: outputFile, inputFile1, inputFile2

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

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) exclusionValue
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

   CALL createFile(outputFile,'fort.22',getLogicalUnit())
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------

   CALL computeError(inputFileUnit1,inputFileUnit2,exclusionValue,numberOfData,errorValue)
   CALL writeOutputFile(outputFileUnit,errorValue,numberOfData)

   CALL closeFile(inputFile1)
   CALL closeFile(inputFile2)
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
 SUBROUTINE computeError(inputFileUnit1,inputFileUnit2,exclusionValue,numberOfData,errorValue)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit1, inputFileUnit2
      INTEGERType, INTENT(OUT) :: numberOfData

      REALType, INTENT(IN)    :: exclusionValue
      REALType, INTENT(OUT)   :: errorValue
      REALType :: xValue, yValue, dataValue1, dataValue2
      REALType, PARAMETER :: tolerance = 1.D-6
!     Body
!     - - -
      numberOfData = izero
      errorValue = zero

101 CONTINUE
       READ(inputFileUnit1,*,END=200,ERR=200) xValue,yValue,dataValue1
       READ(inputFileUnit2,*,END=200,ERR=200) xValue,yValue,dataValue2

       IF ( abs(dataValue1-exclusionValue) < tolerance  * abs(exclusionValue) ) GOTO 101
       IF ( abs(dataValue2-exclusionValue) < tolerance  * abs(exclusionValue) ) GOTO 101

       errorValue = errorValue + ( dataValue1 - dataValue2 )**two
       numberOfData = numberOfData + ione
       GOTO 101

200 CONTINUE

 END SUBROUTINE

! Procedure 2 : write output file
! -------------------------------
 SUBROUTINE writeOutputFile(outputFileUnit,errorValue,numberOfData)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: outputFileUnit, numberOfData
      REALType, INTENT(IN)    :: errorValue

!     Body
!     - - -
       WRITE(outputFileUnit,*) errorValue, numberOfData , sqrt(errorValue/(one*numberOfData))

 END SUBROUTINE


END PROGRAM cverror

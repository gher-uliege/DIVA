PROGRAM cvtotalerror

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
   REALType    ::  errorValue, crossValidationValue, dataVart

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
   WRITE(stdOutput,*) 'Please enter the cross validation value'
   READ(stdInput,*) crossValidationValue

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) crossValidationValue
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

   CALL computeError(inputFileUnit1,inputFileUnit2,numberOfData,errorValue,dataVart)
   CALL writeOutputFile(outputFileUnit,errorValue,numberOfData,dataVart,crossValidationValue)

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
 SUBROUTINE computeError(inputFileUnit1,inputFileUnit2,numberOfData,errorValue,dataVart)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit1, inputFileUnit2
      INTEGERType, INTENT(OUT) :: numberOfData
      INTEGERType :: i1, number1, number2

      REALType, INTENT(OUT)   :: errorValue, dataVart
      REALType :: dataValue1, dataValue2
!     Body
!     - - -
      i1 = izero
      numberOfData = izero
      errorValue = zero
      dataVart = zero

101 CONTINUE
       READ(inputFileUnit1,*,END=200,ERR=200) dataValue1, number1
       READ(inputFileUnit2,*,END=200,ERR=200) dataValue2, number2

       errorValue = errorValue + dataValue1
       dataVart = dataVart + dataValue2 * number2
       numberOfData = numberOfData + number1
       i1 = i1 + number2
       GOTO 101

200 CONTINUE
       dataVart = dataVart / ( one * i1 )

 END SUBROUTINE

! Procedure 2 : write output file
! -------------------------------
 SUBROUTINE writeOutputFile(outputFileUnit,errorValue,numberOfData,dataVart,crossValidationValue)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: outputFileUnit, numberOfData
      REALType, INTENT(IN)    :: errorValue, dataVart, crossValidationValue

!     Body
!     - - -
       WRITE(outputFileUnit,76) crossValidationValue,sqrt(errorValue/(one*numberOfData)),dataVart, &
                                sqrt(errorValue/(one*numberOfData)) ,zero,zero,one*numberOfData
 76   FORMAT(7(E11.4))
 
 END SUBROUTINE


END PROGRAM cvtotalerror

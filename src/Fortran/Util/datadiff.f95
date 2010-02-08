PROGRAM datadiff

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
   INTEGER :: inputFileUnit1, inputFileUnit2, outputFileUnit, nbOfColumn
   REALType    :: exclusionValue

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
   WRITE(stdOutput,*) 'Please enter the number of column in the data file'
   READ(stdInput,*) nbOfColumn
   WRITE(stdOutput,*) 'Please enter the exclusion value'
   READ(stdInput,*) exclusionValue

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfColumn,exclusionValue
30 CONTINUE

#endif

#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Number of column in data file,exclusion value'
   WRITE(stdOutput,*) nbOfColumn,exclusionValue
#endif


!    Opening file to read and to write
!    ---------------------------------
   CALL createFile(inputFile1,'fort.44',formType=STD_FORMATTED)
   CALL openFile(inputFile1)
   inputFileUnit1 = getFileUnit(inputFile1)

   CALL createFile(inputFile2,'fort.45',formType=STD_FORMATTED)
   CALL openFile(inputFile2)
   inputFileUnit2 = getFileUnit(inputFile2)

   CALL createFile(outputFile,'fort.46',formType=STD_FORMATTED)
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------

    SELECT CASE (nbOfColumn)
       CASE (ithree)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '3col'
          WRITE(stdOutput,*) 'Data hence without relative weights'
#endif
          CALL computeForThreeColumn(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)
       CASE (ifour)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '4col'
          WRITE(stdOutput,*) 'Data using relative weights'
#endif
          CALL computeForFourColumn(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)
    END SELECT

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

! Procedure 1 : compute data based on three column data file
! -----------------------------------------------------------
 SUBROUTINE computeForThreeColumn(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit1, inputFileUnit2, outputFileUnit
      REALType, INTENT(IN)    :: exclusionValue
      REALType :: xValue, yValue, dataValue1, dataValue2, difference
      REALType, PARAMETER :: tolerance = 1.D-5

!     Body
!     - - -
      DO
          READ(inputFileUnit1,*,END=100)  xValue, yValue, dataValue1
          READ(inputFileUnit2,*,END=100)  xValue, yValue, dataValue2
          difference = dataValue1 - dataValue2
          
          IF ( abs( dataValue2 - exclusionValue) < tolerance * abs(exclusionValue) ) THEN
             difference = zero
          END IF
          
          WRITE(outputFileUnit,*) xValue, yValue,difference
          
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 2 : compute data based on four column data file
! -----------------------------------------------------------
 SUBROUTINE computeForFourColumn(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit1, inputFileUnit2, outputFileUnit
      REALType, INTENT(IN)    :: exclusionValue
      REALType :: xValue, yValue, dataValue1, dataValue2, weight, difference
      REALType, PARAMETER :: tolerance = 1.D-5

!     Body
!     - - -
      DO
          READ(inputFileUnit1,*,END=100)  xValue, yValue, dataValue1, weight
          READ(inputFileUnit2,*,END=100)  xValue, yValue, dataValue2
          difference = dataValue1 - dataValue2

          IF ( abs( dataValue2 - exclusionValue) < tolerance * abs(exclusionValue) ) THEN
             difference = zero
          END IF

          WRITE(outputFileUnit,*) xValue, yValue,difference, weight

      END DO

 100   CONTINUE

 END SUBROUTINE


END PROGRAM datadiff

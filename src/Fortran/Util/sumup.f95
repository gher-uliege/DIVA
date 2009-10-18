PROGRAM sumup

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
   INTEGER :: inputFileUnit1, inputFileUnit2, outputFileUnit
   REALType    :: exclusionValue

   TYPE(file) :: outputFile, inputFile1, inputFile2

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

   CALL computeSumup(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)

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

! Procedure 1 : compute data based
! --------------------------------
 SUBROUTINE computeSumup(inputFileUnit1,inputFileUnit2,outputFileUnit,exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit1, inputFileUnit2, outputFileUnit
      REALType, INTENT(IN)    :: exclusionValue
      REALType :: xValue, yValue, dataValue1, dataValue2, sum
      REALType, PARAMETER :: tolerance = 1.D-5

!     Body
!     - - -
      DO
          READ(inputFileUnit1,*,END=100)  xValue, yValue, dataValue1
          READ(inputFileUnit2,*,END=100)  xValue, yValue, dataValue2
          sum = dataValue1 + dataValue2

          IF ( abs( dataValue1 - exclusionValue) < tolerance * abs(exclusionValue) ) THEN
             sum = zero
          END IF
          IF ( abs( dataValue2 - exclusionValue) < tolerance * abs(exclusionValue) ) THEN
             sum = zero
          END IF

          WRITE(outputFileUnit,*) xValue, yValue,sum

      END DO

 100   CONTINUE

 END SUBROUTINE

END PROGRAM sumup

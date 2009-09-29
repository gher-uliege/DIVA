
PROGRAM calcest

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
   INTEGERType :: inputFileUnit, outputFileUnit, nbOfColumn
   REALType    :: generalizedCrossValidator, tracea
   
   Type(file) :: outputFile, inputFile

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

!     Read information about the length scale and penalisation for the gradients
!     --------------------------------------------------------------------------
!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the number of column in the data file'
   READ(stdInput,*) nbOfColumn
   WRITE(stdOutput,*) 'Please enter the generalized cross validator value'
   READ(stdInput,*) generalizedCrossValidator
   WRITE(stdOutput,*) 'Please enter the tracea value'
   READ(stdInput,*) tracea

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfColumn,generalizedCrossValidator,tracea
30 CONTINUE

#endif

#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Number of column in data file,generalized cross validator value,tracea'
   WRITE(stdOutput,*) nbOfColumn,generalizedCrossValidator,tracea
#endif
   

!    Opening file to read and to write
!    ---------------------------------
   CALL createFile(inputFile,'fort.44',getLogicalUnit())
   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile)

   CALL createFile(outputFile,'fort.76',getLogicalUnit())
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------
    SELECT CASE (nbOfColumn)
       CASE (ithree)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '3col'
#endif
          CALL computeForThreeColumn(inputFileUnit,outputFileUnit,generalizedCrossValidator,tracea)
       CASE (ifour)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '4col'
#endif
          CALL computeForFourColumn(inputFileUnit,outputFileUnit,generalizedCrossValidator,tracea)
    END SELECT
    
   CALL closeFile(inputFile)
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
 SUBROUTINE computeForThreeColumn(inputFileUnit,outputFileUnit,generalizedCrossValidator,tracea)
 
!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: generalizedCrossValidator, tracea
      REALType :: xValue, yValue, dataValue

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue
          WRITE(outputFileUnit,*) xValue, yValue,generalizedCrossValidator*(one-tracea)
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 2 : compute data based on four column data file
! -----------------------------------------------------------
 SUBROUTINE computeForFourColumn(inputFileUnit,outputFileUnit,generalizedCrossValidator,tracea)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: generalizedCrossValidator, tracea
      REALType :: xValue, yValue, dataValue, weight

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue, weight
          WRITE(outputFileUnit,*) xValue, yValue,generalizedCrossValidator/sqrt(weight)*(one-tracea)
      END DO

 100   CONTINUE

 END SUBROUTINE



END PROGRAM calcest

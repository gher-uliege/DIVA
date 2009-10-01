PROGRAM calcestbis

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
   REALType    :: signalToNoiseRatio, tracea, varianceBackgroundField
   REALType    :: epsilon
   
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

!     Read information
!     ----------------
!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the signal to noise ratio'
   READ(stdInput,*) signalToNoiseRatio
   WRITE(stdOutput,*) 'Please enter the number of column in the data file'
   READ(stdInput,*) nbOfColumn
   WRITE(stdOutput,*) 'Please enter the tracea value'
   READ(stdInput,*) tracea
   WRITE(stdOutput,*) 'Please enter the variance of the background field'
   READ(stdInput,*) varianceBackgroundField

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) signalToNoiseRatio,nbOfColumn,tracea,varianceBackgroundField
30 CONTINUE

#endif

#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Signal to noise ratio,number of column in data file,tracea,variance of the background field'
   WRITE(stdOutput,*) signalToNoiseRatio,nbOfColumn,tracea,varianceBackgroundField
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
    epsilon = computeEpsilon(varianceBackgroundField,signalToNoiseRatio,tracea)


    SELECT CASE (nbOfColumn)
       CASE (ithree)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '3col'
#endif
          CALL computeForThreeColumn(inputFileUnit,outputFileUnit,epsilon)
       CASE (ifour)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '4col'
#endif
          CALL computeForFourColumn(inputFileUnit,outputFileUnit,epsilon)
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
 SUBROUTINE computeForThreeColumn(inputFileUnit,outputFileUnit,epsilon)
 
!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: epsilon
      REALType :: xValue, yValue, dataValue

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue
          WRITE(outputFileUnit,*) xValue, yValue,epsilon
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 2 : compute data based on four column data file
! -----------------------------------------------------------
 SUBROUTINE computeForFourColumn(inputFileUnit,outputFileUnit,epsilon)

!     Declaration
!     - - - - - -
      INTEGERType, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: epsilon
      REALType :: xValue, yValue, dataValue, weight

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue, weight
          WRITE(outputFileUnit,*) xValue, yValue,epsilon/sqrt(weight)
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 3 : function to evaluate epsilon
! ------------------------------------------
 FUNCTION computeEpsilon(varianceBackgroundField,signalToNoiseRatio,tracea) RESULT(epsilon)

!     Declaration
!     - - - - - -
      REALType, INTENT(IN)  ::  varianceBackgroundField,signalToNoiseRatio,tracea
      REALType :: epsilon
      
!     Body
!     - - -
      epsilon =  sqrt(varianceBackgroundField/signalToNoiseRatio*(one-tracea))
      
 END FUNCTION


END PROGRAM calcestbis

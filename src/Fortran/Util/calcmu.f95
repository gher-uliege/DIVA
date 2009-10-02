PROGRAM calcmu

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
   INTEGER :: inputFileUnit, outputFileUnit, nbOfColumn
   REALType    :: lengthScale, signalToNoiseRatio, xi
   REALType    :: valueToWrite, intermediateValue
   
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
   WRITE(stdOutput,*) 'Please enter the length scale'
   READ(stdInput,*) lengthScale
   WRITE(stdOutput,*) 'Please enter the signal to noise ratio'
   READ(stdInput,*) signalToNoiseRatio
   WRITE(stdOutput,*) 'Please enter the number of column in the data file'
   READ(stdInput,*) nbOfColumn
   WRITE(stdOutput,*) 'Please enter the xi value'
   READ(stdInput,*) xi

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) lengthScale,signalToNoiseRatio,nbOfColumn,xi
30 CONTINUE

#endif

#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Lenth scale, Signal to noise ratio,number of column in data file,xi'
   WRITE(stdOutput,*) lengthScale,signalToNoiseRatio,nbOfColumn,xi
#endif
   

!    Opening file to read and to write
!    ---------------------------------
   CALL createFile(inputFile,'fort.44',getLogicalUnit())
   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile)

   CALL createFile(outputFile,'fort.20',getLogicalUnit())
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------
    intermediateValue = computeIntermediateValue(xi)
    valueToWrite = computeValueToWrite(intermediateValue,signalToNoiseRatio,lengthScale)


    SELECT CASE (nbOfColumn)
       CASE (ithree)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '3col'
          WRITE(stdOutput,*) 'Data hence without relative weights'
#endif
          CALL computeForThreeColumn(inputFileUnit,outputFileUnit,valueToWrite)
       CASE (ifour)
#ifdef _INTERACTIVE_MODE_
          WRITE(stdOutput,*) 'number of column in data file'
          WRITE(stdOutput,*) '4col'
          WRITE(stdOutput,*) 'Data using relative weights'
#endif
          CALL computeForFourColumn(inputFileUnit,outputFileUnit,valueToWrite)
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
 SUBROUTINE computeForThreeColumn(inputFileUnit,outputFileUnit,valueToWrite)
 
!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: valueToWrite
      REALType :: xValue, yValue, dataValue

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue
          WRITE(outputFileUnit,*) xValue, yValue,valueToWrite
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 2 : compute data based on four column data file
! -----------------------------------------------------------
 SUBROUTINE computeForFourColumn(inputFileUnit,outputFileUnit,valueToWrite)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit, outputFileUnit
      REALType, INTENT(IN)    :: valueToWrite
      REALType :: xValue, yValue, dataValue, weight

!     Body
!     - - -
      DO
          READ(inputFileUnit,*,END=100)  xValue, yValue, dataValue, weight
          WRITE(outputFileUnit,*) xValue, yValue,weight*valueToWrite
      END DO

 100   CONTINUE

 END SUBROUTINE

! Procedure 3 : function to evaluate the value to write
! -----------------------------------------------------
 FUNCTION computeValueToWrite(intermediateValue,signalToNoiseRatio,lengthScale) RESULT(valueToWrite)

!     Declaration
!     - - - - - -
      REALType, INTENT(IN)  ::  intermediateValue,signalToNoiseRatio,lengthScale
      REALType :: valueToWrite
      
!     Body
!     - - -
      valueToWrite =  intermediateValue * signalToNoiseRatio / ( lengthScale * lengthScale )
      
 END FUNCTION

! Procedure 4 : function to evaluate the intermediate value
! ---------------------------------------------------------
 FUNCTION computeIntermediateValue(xi) RESULT(intermediateValue)

!     Declaration
!     - - - - - -
      REALType, INTENT(IN)  ::  xi
      REALType :: intermediateValue, angle

!     Body
!     - - -
      intermediateValue =  sixteen * Atan(one)
      
      IF ( abs(xi) > one ) THEN
          angle = log(abs(xi)+sqrt(xi*xi-one))
          IF ( angle /= zero ) THEN
              intermediateValue = sixteen * Atan(one) * sinh(angle) / angle
          END IF
      ELSE
          angle = acos(xi)
          IF ( angle /= zero ) THEN
              intermediateValue = sixteen * Atan(one) * sin(angle) / angle
          END IF
      END IF

 END FUNCTION

END PROGRAM calcmu

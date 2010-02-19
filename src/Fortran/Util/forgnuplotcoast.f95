PROGRAM forGnuPlotCoast

! Module
! ======
  USE moduleDIVA
  USE moduleFile

! Declaration
! ===========
   INTEGER :: nbOfContour, nbOfBoundarySegment, inputFileUnit, outputFileUnit, i1, i2
   Type(file) :: inputFile, outputFile
   REAL(KIND=4) :: xValue, yValue

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
   CALL createFile(inputFile,'fort.66',formType=STD_FORMATTED)
   CALL createFile(outputFile,'fort.67',formType=STD_FORMATTED)

   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile) 

   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)
   
   READ(inputFileUnit,*) nbOfContour
   
   DO i1 = 1, nbOfContour
      READ(inputFileUnit,*) nbOfBoundarySegment
      DO i2 = 1, nbOfBoundarySegment
         READ(inputFileUnit,*) xValue, yValue
         WRITE(outputFileUnit,*) xValue, yValue
      ENDDO
      WRITE(outputFileUnit,*)
      WRITE(outputFileUnit,*)
   ENDDO
   
   CALL closeFile(inputFile) 
   CALL closeFile(outputFile) 

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

  
END PROGRAM forGnuPlotCoast

PROGRAM forGnuPlotMeshl

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE vectorInterface
  USE nodeInterface

! Declaration
! ===========
   INTEGER :: nbOfVertices, nbOfBlank, nbOfCell, inputFileUnit1, inputFileUnit2, outputFileUnit, i1, i2, &
              iNode1, iNode2, iNode3
   TYPE(file) :: inputFile1, inputFile2, outputFile
   REAL(KIND=8), PARAMETER :: FAC = 1.0
   REAL(KIND=8) :: xValue1, yValue1, xValue2, yValue2, xValue3, yValue3, xValueMean, yValueMean  
   TYPE(nodeDataBase) :: vertices
   TYPE(node), POINTER :: ptrNode

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
   CALL createFile(inputFile1,'fort.23',formType=STD_FORMATTED)
   CALL createFile(inputFile2,'fort.22',formType=STD_FORMATTED)
   CALL createFile(outputFile,'fort.71',formType=STD_FORMATTED)

!    1) Opening files
!    ----------------
   CALL openFile(inputFile1)
   inputFileUnit1 = getFileUnit(inputFile1) 
   CALL openFile(inputFile2)
   inputFileUnit2 = getFileUnit(inputFile2) 

   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)
   
!    2) Reading dimension
!    --------------------   
   READ(inputFileUnit1,*) nbOfVertices
   READ(inputFileUnit1,*) nbOfBlank
   READ(inputFileUnit1,*) nbOfCell
   
!    3) Reading nodes
!    ----------------
   CALL nodeDBCreate(vertices,nbOfVertices)

   DO i1 = 1, nbOfVertices
      ptrNode => vertices%values(i1)
      ptrNode%indexValue = i1
      READ(inputFileUnit2,*) i2, ptrNode%xValue, ptrNode%yValue
   ENDDO

!    4) Reading blank
!    ----------------
   DO i2 = 1, nbOfBlank
      READ(inputFileUnit2,*)
   ENDDO
   
!    5) Reading and writing cells
!    ----------------------------
   DO i1 = 1, nbOfCell
      READ(inputFileUnit2,*) iNode1, i2, iNode2, i2, iNode3, i2
      
      ptrNode => vertices%values(iNode1)
      xValue1 = ptrNode%xValue
      yValue1 = ptrNode%yValue
      
      ptrNode => vertices%values(iNode2)
      xValue2 = ptrNode%xValue
      yValue2 = ptrNode%yValue
      
      ptrNode => vertices%values(iNode3)
      xValue3 = ptrNode%xValue
      yValue3 = ptrNode%yValue
      
      xValueMean = ( xValue1 + xValue2 + xValue3 ) / 3.0
      yValueMean = ( yValue1 + yValue2 + yValue3 ) / 3.0
      
      xValue1 = xValueMean + ( xValue1 - xValueMean  ) * FAC
      xValue2 = xValueMean + ( xValue2 - xValueMean  ) * FAC
      xValue3 = xValueMean + ( xValue3 - xValueMean  ) * FAC

      yValue1 = yValueMean + ( yValue1 - yValueMean  ) * FAC
      yValue2 = yValueMean + ( yValue2 - yValueMean  ) * FAC
      yValue3 = yValueMean + ( yValue3 - yValueMean  ) * FAC

      WRITE(outputFileUnit,*) xValue1, yValue1
      WRITE(outputFileUnit,*) xValue2, yValue2
      WRITE(outputFileUnit,*) xValue3, yValue3
      WRITE(outputFileUnit,*) 

   ENDDO   
   
!    6) Close files
!    ---------------   
   CALL closeFile(inputFile1) 
   CALL closeFile(inputFile2) 
   CALL closeFile(outputFile) 

!  Always finalise the DIVA context
!  ================================
   CALL nodeDBDestroy(vertices)   
   CALL finaliseDIVAContext()

  
END PROGRAM forGnuPlotMeshl
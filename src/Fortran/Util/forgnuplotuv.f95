PROGRAM forGnuPlotUV

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface

! Declaration
! ===========
   REAL(KIND=4) :: exclusionValueField
   INTEGER :: iMax1, jMax1, iMax2, jMax2

   Type(file) :: inputFile1, inputFile2, inputFile3
   Type(file) :: outputFile
   TYPE(arrayReal4) :: fieldU, fieldV
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrFieldU, ptrFieldV

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
!     1) Creation of array to store fieldU and fieldV
!     -----------------------------------------------
   CALL arrayCreate(fieldU)
   CALL arrayCreate(fieldV)

!     2) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.30',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile2,'fort.31',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile3,'fort.32',formType=STD_FORMATTED)

   CALL createFile(outputFile,'fort.55',formType=STD_FORMATTED)
   
!     3) Reading fieldU and field2
!     ----------------------------   
   CALL arrayRead(fieldU,inputFile1,exclusionValueField)
   CALL arrayRead(fieldV,inputFile2,exclusionValueField)
   
!     4) Check how many gridded field
!     -------------------------------
   iMax1 = arrayGetSizeX(fieldU)
   jMax1 = arrayGetSizeY(fieldU)

   iMax2 = arrayGetSizeX(fieldV)
   jMax2 = arrayGetSizeY(fieldV)
     
   IF ( (iMax1/=iMax2).OR.(jMax1/=jMax2) ) THEN
      STOP 'Error of dimension in fieldU or fieldV'
   ENDIF
   
!     5) Write to gnu format (only for k = 1)
!     ---------------------------------------
   
   ptrFieldU => arrayGetValues(fieldU)
   ptrFieldV => arrayGetValues(fieldV)
   
   CALL convertToGnu(ptrFieldU(1:iMax1,1:jMax1,1),ptrFieldV(1:iMax1,1:jMax1,1),iMax1,jMax1, &
                     exclusionValueField,inputFile3,outputFile)
         
!  Always finalise the DIVA context
!  ================================
   CALL arrayDestroy(fieldU)      
   CALL arrayDestroy(fieldV)      
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

! Procedure 1 : convert to Gnu
! ----------------------------
SUBROUTINE convertToGnu(ptrFieldU,ptrFieldV,iMax,jMax,exclusionValueField, &
                        inputFile,outputFile)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iMax, jMax
      REAL(KIND=4), INTENT(IN) :: exclusionValueField
      REAL(KIND=4), DIMENSION(:,:), TARGET :: ptrFieldU, ptrFieldV
      TYPE(file) :: inputFile, outputFile
      
      INTEGER :: inputFileUnit, outputFileUnit, checkScaling
      INTEGER :: nbOfGridPointX, nbOfGridPointY, i1, i2
      REAL(KIND=4) :: gridOriginX, gridOriginY, gridStepX, gridStepY, fieldVscalingFactor, scalingFactor, &
                      xCoordinateP1,yCoordinateP1

      REAL(KIND=4), POINTER :: ptrValueFieldU, ptrValueFieldV

!     Body
!     - - -

!        1) Reading input file
!        + + + + + + + + + + +
     CALL openFile(inputFile)
     inputFileUnit = getFileUnit(inputFile) 
     
     READ(inputFileUnit,*) gridOriginX
     READ(inputFileUnit,*) gridOriginY
     READ(inputFileUnit,*) gridStepX
     READ(inputFileUnit,*) gridStepY
     READ(inputFileUnit,*) nbOfGridPointX
     READ(inputFileUnit,*) nbOfGridPointY

     CALL closeFile(inputFile)
     
     PRINT*,'valex', exclusionValueField, nbOfGridPointX, nbOfGridPointY, gridStepX, gridStepY
     
     IF ( nbOfGridPointX /= iMax ) THEN
        STOP 'incoherent files'
     ENDIF     
     IF ( nbOfGridPointY /= jMax ) THEN
        STOP 'incoherent files'
     ENDIF     

!       2) Computing scaling factor
!       + + + + + + + + + + + + + + +
     fieldVscalingFactor = 0.
     checkScaling = 0
     
     DO i1 = 1, nbOfGridPointX
      DO i2 = 1, nbOfGridPointY
        ptrValueFieldV => ptrFieldV(i1,i2)
        IF ( ptrValueFieldV /= exclusionValueField ) THEN
           ptrValueFieldU => ptrFieldU(i1,i2)       
           checkScaling = checkScaling + 1
           fieldVscalingFactor = fieldVscalingFactor + ptrValueFieldV * ptrValueFieldV + ptrValueFieldU * ptrValueFieldU
        ENDIF
      ENDDO
     ENDDO
     
     fieldVscalingFactor = sqrt(fieldVscalingFactor/checkScaling)
     scalingFactor = sqrt(gridStepX*gridStepY)/(2.0*fieldVscalingFactor)

!       3) Writing result
!       + + + + + + + + +
     CALL openFile(outputFile)
     outputFileUnit = getFileUnit(outputFile)
     
     DO i1 = 1, nbOfGridPointX
      DO i2 = 1, nbOfGridPointY
        ptrValueFieldV => ptrFieldV(i1,i2)
        IF ( ptrValueFieldV /= exclusionValueField ) THEN
           ptrValueFieldU => ptrFieldU(i1,i2)
           xCoordinateP1 = gridOriginX + ( i1 - 1 ) * gridStepX
           yCoordinateP1 = gridOriginY + ( i2 - 1 ) * gridStepY
           
           WRITE(outputFileUnit,*) xCoordinateP1, yCoordinateP1, ptrValueFieldU * scalingFactor, ptrValueFieldV * scalingFactor 
                             
        ENDIF
      ENDDO
     ENDDO
       
     CALL closeFile(outputFile)
     
END SUBROUTINE
   
END PROGRAM forGnuPlotUV

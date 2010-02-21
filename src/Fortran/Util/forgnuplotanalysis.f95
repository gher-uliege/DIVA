PROGRAM forGnuPlotAnalysis

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface

! Declaration
! ===========
   REAL(KIND=4) :: exclusionValueGridField1, exclusionValueGridField2
   INTEGER :: i1, i2, i3, iMax1, jMax1, kMax1, iMax2, jMax2, kMax2

   TYPE(file) :: inputFile1, inputFile2, inputFile3
   TYPE(file) :: outputFile40, outputFile41, outputFile47, outputFile68
   TYPE(arrayReal4) :: gridField1, gridField2
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrGridField1, ptrGridField2
   REAL(KIND=4), POINTER :: ptrValueGridField2

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
!     1) Creation of array to store gridField1 and gridField2
!     -------------------------------------------------------
   CALL arrayCreate(gridField1)
   CALL arrayCreate(gridField2)

!     2) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.20',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile2,'fort.19',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile3,'fort.21',formType=STD_FORMATTED)
   CALL createFile(outputFile40,'fort.40',formType=STD_FORMATTED)
   CALL createFile(outputFile41,'fort.41',formType=STD_FORMATTED)
   CALL createFile(outputFile47,'fort.47',formType=STD_FORMATTED)
   CALL createFile(outputFile68,'fort.68',formType=STD_FORMATTED)
   
!     3) Reading Grid1 and Grid2
!     ----------------------------   
   PRINT*, 'Into reading'
   CALL arrayRead(gridField1,inputFile1,exclusionValueGridField1)
   PRINT*, 'Have read a gridded field'
   CALL arrayRead(gridField2,inputFile2,exclusionValueGridField2)
   
!     4) Check how many gridded field
!     -------------------------------
   iMax1 = arrayGetSizeX(gridField1)
   jMax1 = arrayGetSizeY(gridField1)
   kMax1 = arrayGetSizeZ(gridField1)

   iMax2 = arrayGetSizeX(gridField2)
   jMax2 = arrayGetSizeY(gridField2)
   kMax2 = arrayGetSizeZ(gridField2)
      
   IF ( (iMax1==iMax2).AND.(jMax1==jMax2).AND.(kMax1==kMax2) ) THEN

      PRINT*, 'Found a second gridded field'
      ptrGridField2 => arrayGetValues(gridField2)
      
!$OMP PARALLEL DEFAULT(NONE) SHARED(iMax1,jMax1,kMax1,exclusionValueGridField1,exclusionValueGridField2,      &
!$OMP                               ptrGridField2) & 
!$OMP                        PRIVATE(i1,i2,i3,ptrValueGridField2)
!$OMP DO

      DO i3 = 1, kMax1
       DO i2 = 1, jMax1
        DO i1 = 1, iMax1
           ptrValueGridField2 => ptrGridField2(i1,i2,i3)
           IF ( ptrValueGridField2 == exclusionValueGridField2 ) THEN
             ptrValueGridField2 = exclusionValueGridField1
           ENDIF
        ENDDO
       ENDDO
      ENDDO
     
!$OMP END DO
!$OMP END PARALLEL      

   ELSE
   
      PRINT*, ' Found only one gridded file'
      CALL arraySetSize(gridField2,iMax1,jMax1,kMax1)
      CALL arraySetToZero(gridField2)
   
   ENDIF
   
!     5) Write to gnu format (only for k = 1)
!     ---------------------------------------
   IF ( kMax1 > 1 ) THEN
      PRINT*,'3D files; using k=1'  
   ENDIF
   
   ptrGridField1 => arrayGetValues(gridField1)
   ptrGridField2 => arrayGetValues(gridField2)
   
   CALL convertToGnu(ptrGridField1(1:iMax1,1:jMax1,1),ptrGridField2(1:iMax1,1:jMax1,1),iMax1,jMax1, &
                     exclusionValueGridField1,inputFile3,outputFile40,outputFile41,outputFile47,outputFile68)
         
!  Always finalise the DIVA context
!  ================================
   CALL arrayDestroy(gridField1)      
   CALL arrayDestroy(gridField2)      
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
SUBROUTINE convertToGnu(ptrGridField1,ptrGridField2,iMax,jMax,exclusionValueGridField, &
                        inputFile,outputFile40,outputFile41,outputFile47,outputFile68)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iMax, jMax
      REAL(KIND=4), INTENT(IN) :: exclusionValueGridField
      REAL(KIND=4), DIMENSION(:,:), TARGET :: ptrGridField1, ptrGridField2
      TYPE(file) :: inputFile, outputFile40, outputFile41, outputFile47, outputFile68
      
      INTEGER :: inputFileUnit, outputFileUnit
      INTEGER :: nbOfGridPointX, nbOfGridPointY, i1, i2
      REAL(KIND=4) :: gridOriginX, gridOriginY, gridStepX, gridStepY, &
                      xCoordinateP1, xCoordinateP2, xCoordinateP3, xCoordinateP4, &
                      yCoordinateP1, yCoordinateP2, yCoordinateP3, yCoordinateP4

      REAL(KIND=4), POINTER :: ptrValueGridField1, ptrValueGridField2

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
     
     PRINT*,'valex', exclusionValueGridField, nbOfGridPointX, nbOfGridPointY, gridStepX, gridStepY
     
     IF ( nbOfGridPointX /= iMax ) THEN
        STOP 'incoherent files'
     ENDIF     
     IF ( nbOfGridPointY /= jMax ) THEN
        STOP 'incoherent files'
     ENDIF     

!       2) Writing first output file
!       + + + + + + + + + + + + + + +
     CALL openFile(outputFile68)
     outputFileUnit = getFileUnit(outputFile68)
     
     DO i1 = 1, nbOfGridPointX
      DO i2 = 1, nbOfGridPointY
        ptrValueGridField1 => ptrGridField1(i1,i2)
        IF ( ptrValueGridField1 /= exclusionValueGridField ) THEN
          xCoordinateP1 = gridOriginX + ( i1 - 1 ) * gridStepX - 0.5 * gridStepX
          yCoordinateP1 = gridOriginY + ( i2 - 1 ) * gridStepY - 0.5 * gridStepY

          xCoordinateP2 = xCoordinateP1 + gridStepX
          yCoordinateP2 = yCoordinateP1
          
          xCoordinateP3 = xCoordinateP2
          yCoordinateP3 = yCoordinateP2 + gridStepY

          xCoordinateP4 = xCoordinateP1
          yCoordinateP4 = yCoordinateP3
          
          ptrValueGridField2 => ptrGridField2(i1,i2)
          
          WRITE(outputFileUnit,*) '#'
          WRITE(outputFileUnit,*) xCoordinateP1, yCoordinateP1, ptrValueGridField1, ptrValueGridField2
          WRITE(outputFileUnit,*) xCoordinateP2, yCoordinateP2, ptrValueGridField1, ptrValueGridField2
          WRITE(outputFileUnit,*)
          WRITE(outputFileUnit,*) xCoordinateP4, yCoordinateP4, ptrValueGridField1, ptrValueGridField2
          WRITE(outputFileUnit,*) xCoordinateP3, yCoordinateP3, ptrValueGridField1, ptrValueGridField2
          WRITE(outputFileUnit,*)
          WRITE(outputFileUnit,*)
          
        ENDIF
      ENDDO
     ENDDO
     
     CALL closeFile(outputFile68)
     
!       3) Writing second output file
!       + + + + + + + + + + + + + + +
     CALL openFile(outputFile41)
     outputFileUnit = getFileUnit(outputFile41)

     xCoordinateP1 = gridOriginX     
     yCoordinateP1 = gridOriginY     

     xCoordinateP3 = gridOriginX + ( nbOfGridPointX - 1 ) * gridStepX   
     yCoordinateP3 = gridOriginY + ( nbOfGridPointY - 1 ) * gridStepY
     
     WRITE(outputFileUnit,*) 'set cbrange[', &
               minval(ptrGridField1(1:iMax1,1:jMax1),MASK=ptrGridField1/=exclusionValueGridField),':', &
               maxval(ptrGridField1(1:iMax1,1:jMax1),MASK=ptrGridField1/=exclusionValueGridField),']'
     WRITE(outputFileUnit,*) 'set xrange[',xCoordinateP1,':',xCoordinateP3,']'
     WRITE(outputFileUnit,*) 'set yrange[',yCoordinateP1,':',yCoordinateP3,']'
     
     CALL closeFile(outputFile41)

!       4) Writing third output file
!       + + + + + + + + + + + + + + +
     CALL openFile(outputFile40)
     outputFileUnit = getFileUnit(outputFile40)
     
     WRITE(outputFileUnit,*) 'longref=',gridOriginX + ( nbOfGridPointX / 2 - 1 ) * gridStepX
     
     CALL closeFile(outputFile40)
     
!       5) Writing fourth output file
!       + + + + + + + + + + + + + + +
     CALL openFile(outputFile47)
     outputFileUnit = getFileUnit(outputFile47)
     
     WRITE(outputFileUnit,*) '      <north>',yCoordinateP3,'</north>'
     WRITE(outputFileUnit,*) '      <south>',yCoordinateP1,'</south>'
     WRITE(outputFileUnit,*) '      <east>',xCoordinateP3,'</east>'
     WRITE(outputFileUnit,*) '      <west>',xCoordinateP1,'</west>'
     
     CALL closeFile(outputFile47)
     
END SUBROUTINE
   
END PROGRAM forGnuPlotAnalysis

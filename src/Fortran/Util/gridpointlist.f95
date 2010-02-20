PROGRAM gridPointList

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface

! Declaration
! ===========
   REAL(KIND=4) :: exclusionValueGridField
   INTEGER :: iMax, jMax, kMax

   Type(file) :: inputFile1, inputFile2
   Type(file) :: outputFile
   TYPE(arrayReal4) :: gridField
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrGridField

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
   CALL arrayCreate(gridField)

!     2) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.20',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile2,'fort.21',formType=STD_FORMATTED)
   CALL createFile(outputFile,'fort.22',formType=STD_FORMATTED)
   
!     3) Reading Grid1
!     ----------------   
   PRINT*, 'Into reading'
   CALL arrayRead(gridField,inputFile1,exclusionValueGridField)
   PRINT*, 'Have read a gridded field'
   
!     4) Write to gnu format (only for k = 1)
!     -----------------------
   iMax = arrayGetSizeX(gridField)
   jMax = arrayGetSizeY(gridField)
   kMax = arrayGetSizeZ(gridField)

   IF ( kMax > 1 ) THEN
      PRINT*,'3D files; using k=1'  
   ENDIF
   
   ptrGridField => arrayGetValues(gridField)
   
   CALL convertToGnu(ptrGridField(1:iMax,1:jMax,1),iMax,jMax,exclusionValueGridField,inputFile2,outputFile)
         
!  Always finalise the DIVA context
!  ================================
   CALL arrayDestroy(gridField)      
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
SUBROUTINE convertToGnu(ptrGridField,iMax,jMax,exclusionValueGridField,inputFile,outputFile)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iMax, jMax
      REAL(KIND=4), INTENT(IN) :: exclusionValueGridField
      REAL(KIND=4), DIMENSION(:,:), TARGET :: ptrGridField
      TYPE(file) :: inputFile, outputFile
      
      INTEGER :: inputFileUnit, outputFileUnit
      INTEGER :: nbOfGridPointX, nbOfGridPointY, i1, i2
      REAL(KIND=4) :: gridOriginX, gridOriginY, gridStepX, gridStepY

      REAL(KIND=4), POINTER :: ptrValueGridField

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
     CALL openFile(outputFile)
     outputFileUnit = getFileUnit(outputFile)
     
     DO i1 = 1, nbOfGridPointX
      DO i2 = 1, nbOfGridPointY
        ptrValueGridField => ptrGridField(i1,i2)
        IF ( ptrValueGridField /= exclusionValueGridField ) THEN
          WRITE(outputFileUnit,*) gridOriginX+(i1-1)*gridStepX,gridOriginY+(i2-1)*gridStepY,ptrValueGridField,1
        ENDIF
      ENDDO
     ENDDO
     
     CALL closeFile(outputFile)
     
END SUBROUTINE
   
END PROGRAM gridPointList

PROGRAM gherMask

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface

! Declaration
! ===========
   REAL(KIND=4) :: exclusionValueGridField1, exclusionValueGridField2
   INTEGER :: iMax1, jMax1, kMax1, iMax2, jMax2, kMax2

   Type(file) :: inputFile1, inputFile2
   Type(file) :: outputFile
   TYPE(arrayReal4) :: gridField1, gridField2
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrGridField1, ptrGridField2

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
   CALL createFile(inputFile1,'fort.99',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile2,'fort.98',formType=GHER_UNFORMATTED)
   CALL createFile(outputFile,'fort.97',formType=GHER_UNFORMATTED)
   
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
      PRINT*, 'Successfully read two files',iMax1,jMax1
      ptrGridField1 => arrayGetValues(gridField1)
      ptrGridField2 => arrayGetValues(gridField2)
      CALL convertToGnu(ptrGridField1(1:iMax1,1:jMax1,1),ptrGridField2(1:iMax1,1:jMax1,1),iMax1,jMax1, &
                     exclusionValueGridField1,exclusionValueGridField2)
   ELSE  
      PRINT*, ' Problem: Found only one gridded file'  
   ENDIF


!     5) Writing file
!     ---------------   
   CALL arrayWrite(gridField2,outputFile,exclusionValueGridField2)
   
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
SUBROUTINE convertToGnu(ptrGridField1,ptrGridField2,iMax,jMax,exclusionValueGridField1,exclusionValueGridField2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iMax, jMax
      REAL(KIND=4), INTENT(IN) :: exclusionValueGridField1, exclusionValueGridField2
      REAL(KIND=4), DIMENSION(:,:) :: ptrGridField1, ptrGridField2
      
      INTEGER :: i1, i2
      
!     Body
!     - - -

!        1) Computing
!        + + + + + + +
!$OMP PARALLEL DEFAULT(NONE) SHARED(iMax,jMax,exclusionValueGridField1,exclusionValueGridField2,      &
!$OMP                               ptrGridField1,ptrGridField2) & 
!$OMP                        PRIVATE(i1,i2)
!$OMP DO

       DO i2 = 1, jMax
        DO i1 = 1, iMax
           IF ( ptrGridField1(i1,i2) == 0. ) THEN
             ptrGridField2(i1,i2) = exclusionValueGridField2
           ENDIF
        ENDDO
       ENDDO
     
!$OMP END DO
!$OMP END PARALLEL      
     
END SUBROUTINE
   
END PROGRAM gherMask

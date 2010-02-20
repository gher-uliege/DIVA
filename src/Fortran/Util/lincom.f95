PROGRAM lincom

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface

! Declaration
! ===========
   INTEGER :: inputFileUnit3
   REAL(KIND=4) :: exclusionValueFieldU, exclusionValueFieldV
   REAL(KIND=4) :: coeffA1, coeffA2, coeffA3, coeffA4
   REAL(KIND=4) :: coeffB1, coeffB2, coeffB3, coeffB4

   TYPE(file) :: outputFile1, outputFile2 , inputFile1, inputFile2, inputFile3
   TYPE(arrayReal4) :: fieldU, fieldV

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
   CALL createFile(inputFile1,'fort.10',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile2,'fort.11',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile3,'fort.20',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'fort.12',formType=GHER_UNFORMATTED)
   CALL createFile(outputFile2,'fort.13',formType=GHER_UNFORMATTED)
   
!     3) Reading fieldU and fieldV
!     ----------------------------   
   CALL arrayRead(fieldU,inputFile1,exclusionValueFieldU)
   CALL arrayRead(fieldV,inputFile2,exclusionValueFieldV)
   
   PRINT*, ' VALEUR D EXCLUSION POUR B: ',exclusionValueFieldU
   PRINT*, ' VALEUR D EXCLUSION POUR X: ',exclusionValueFieldV

!     4) Reading coefficients to modify fieldU and fieldV
!     ---------------------------------------------------
   CALL openFile(inputFile3)
   inputFileUnit3 = getFileUnit(inputFile3)
   
   READ(inputFileUnit3,*) coeffA1, coeffB1
   READ(inputFileUnit3,*) coeffA2, coeffB2
   READ(inputFileUnit3,*) coeffA3, coeffB3
   READ(inputFileUnit3,*) coeffA4, coeffB4
   
   CALL closeFile(inputFile3)

!     5) Modification of fields
!     -------------------------
   CALL uSum(fieldU,fieldV,exclusionValueFieldU,coeffA1,coeffA2,coeffA3,coeffA4,coeffB1,coeffB2,coeffB3,coeffB4)

!     6) Writing modified fields
!     --------------------------
   CALL arrayWrite(fieldU,outputFile1,exclusionValueFieldU)
   CALL arrayWrite(fieldV,outputFile2,exclusionValueFieldV)
      
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

! Procedure 1 : compute modified fields
! -------------------------------------
SUBROUTINE uSum(fieldU,fieldV,exclusionValueFieldU,coeffA1,coeffA2,coeffA3,coeffA4,coeffB1,coeffB2,coeffB3,coeffB4)

!     Declaration
!     - - - - - -
      INTEGER :: i1, i2, i3, iMaxU, jMaxU, kMaxU, iMaxV, jMaxV, kMaxV
      REAL(KIND=4), INTENT(IN) :: exclusionValueFieldU
      REAL(KIND=4), INTENT(IN) :: coeffA1, coeffA2, coeffA3, coeffA4
      REAL(KIND=4), INTENT(IN) :: coeffB1, coeffB2, coeffB3, coeffB4
      TYPE(arrayReal4), INTENT(INOUT) :: fieldU, fieldV
      REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrFieldU, ptrFieldV
      REAL(KIND=4), POINTER :: ptrFieldUValue, ptrFieldVValue
      REAL(KIND=4) :: value1, value2

!     Body
!     - - -
      ptrFieldU => arrayGetValues(fieldU)
      ptrFieldV => arrayGetValues(fieldV)
      
      iMaxU = arrayGetSizeX(fieldU)
      jMaxU = arrayGetSizeY(fieldU)
      kMaxU = arrayGetSizeZ(fieldU)

      iMaxV = arrayGetSizeX(fieldV)
      jMaxV = arrayGetSizeY(fieldV)
      kMaxV = arrayGetSizeZ(fieldV)
      
      IF ( iMaxV /= iMaxU ) THEN
         STOP 'Error in matrix dimension : iMax1 /= iMax2'
      ENDIF
      IF ( jMaxV /= jMaxU ) THEN
         STOP 'Error in matrix dimension : jMax1 /= jMax2'
      ENDIF
      IF ( kMaxV /= kMaxU ) THEN
         STOP 'Error in matrix dimension : kMax1 /= kMax2'
      ENDIF

!$OMP PARALLEL DEFAULT(NONE) SHARED(ptrFieldU,ptrFieldV,iMaxU,jMaxU,kMaxU,exclusionValueFieldU,      &
!$OMP                               coeffA1,coeffA2,coeffA3,coeffA4,coeffB1,coeffB2,coeffB3,coeffB4) & 
!$OMP                        PRIVATE(i1,i2,i3,ptrFieldUValue,ptrFieldVValue,value1,value2)
!$OMP DO

      DO i3 = 1, kMaxU
        DO i2 = 1, jMaxU
          DO i1 = 1, iMaxU
            ptrFieldUValue => ptrFieldU(i1,i2,i3)
            IF ( ptrFieldUValue /= exclusionValueFieldU ) THEN
               ptrFieldVValue => ptrFieldV(i1,i2,i3)
               
               value1 = ptrFieldUValue + coeffA1 * ptrFieldVValue + coeffB1
               value2 = ptrFieldVValue + coeffA2 * ptrFieldUValue + coeffB2
               
               ptrFieldUValue = value1 * coeffB3 + coeffA3
               ptrFieldVValue = value2 * coeffB4 + coeffA4
            
            ENDIF         
          ENDDO
        ENDDO
      ENDDO 
      
!$OMP END DO
!$OMP END PARALLEL      
      
END SUBROUTINE

END PROGRAM lincom

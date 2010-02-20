PROGRAM sumgrid

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE matrixInterface

! Include file
! ============
   INCLUDE 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: inputFileUnit1, inputFileUnit2, outputFileUnit1, nbOfRow, nbOfColumn 
   REAL(KIND=4) :: exclusionValue

   TYPE(file) :: outputFile1, inputFile1, inputFile2
   TYPE(matrixReal4) :: fieldA, fieldB, fieldC

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
!     1) read general dimension
!     -------------------------
#ifdef _BATCH_MODE_
#undef _INTERACTIVE_MODE_
#endif

!       1.1) In interactive mode
!       - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the exclusion value'
   READ(stdInput,*) exclusionValue
   WRITE(stdOutput,*) 'Please enter the dimension of the matrices'
   WRITE(stdOutput,*) '   number of rows :'
   READ(stdInput,*) nbOfRow
   WRITE(stdOutput,*) '   number of columns :'
   READ(stdInput,*) nbOfColumn

!       1.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) exclusionValue,nbOfRow,nbOfColumn
   GOTO 31
30 CONTINUE
   STOP 'Error in reading exclusion value, or matrices dimensions'
31 CONTINUE
#endif
   
!     2) Creation of array to store fieldU and fieldV
!     -----------------------------------------------
   CALL matrixCreate(fieldA,nbOfRow,nbOfColumn)
   CALL matrixCreate(fieldB,nbOfRow,nbOfColumn)
   CALL matrixCreate(fieldC,nbOfRow,nbOfColumn)

!     3) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.20',formType=STD_FORMATTED)
   CALL createFile(inputFile2,'fort.21',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'fort.22',formType=STD_FORMATTED)
   
!     4) Reading fieldA and fieldB
!     ----------------------------   
   CALL openFile(inputFile1)
   inputFileUnit1 = getFileUnit(inputFile1)
   CALL readMatrix(fieldA,nbOfRow,nbOfColumn,inputFileUnit1)
   CALL closeFile(inputFile1)
   
   CALL openFile(inputFile2)
   inputFileUnit2 = getFileUnit(inputFile2)
   CALL readMatrix(fieldB,nbOfRow,nbOfColumn,inputFileUnit2)
   CALL closeFile(inputFile2)

!     5) Computing sum
!     ----------------

   CALL computeSum(fieldA,fieldB,fieldC,nbOfRow,nbOfColumn,exclusionValue)

!     6) Writing result
!     -----------------
   CALL openFile(outputFile1)
   outputFileUnit1 = getFileUnit(outputFile1)
   CALL writeMatrix(fieldC,nbOfRow,nbOfColumn,outputFileUnit1)
   CALL closeFile(outputFile1)

!  Always finalise the DIVA context
!  ================================
   CALL matrixDestroy(fieldA)      
   CALL matrixDestroy(fieldB)      
   CALL matrixDestroy(fieldC)      
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

! Procedure 1 : reading data
! --------------------------
SUBROUTINE readMatrix(field,nbOfRow,nbOfColumn,inputFileUnit)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfColumn,nbOfRow,inputFileUnit
      TYPE(matrixReal4), INTENT(INOUT) :: field
      REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrField
      INTEGER, DIMENSION(9) :: interm
      
      INTEGER :: k, kk, i1, i2, i3

!     Body
!     - - -
      ptrField => matrixGetValues(field)
      
      k = 1
      
1     CONTINUE
      kk = k + 7

      IF ( kk <= nbOfColumn ) THEN
         GOTO 3
      ENDIF

      kk = nbOfColumn
      
3     CONTINUE            

      READ(inputFileUnit,*) (interm(i1),i1=1,kk-k+2) 

      DO i1 = 1, nbOfRow
         READ(inputFileUnit,*) i2, (ptrField(i1,i3),i3=k,kk)
      ENDDO

      k = k + 8      

      IF ( k <= nbOfColumn ) THEN
         GOTO 1
      ENDIF

END SUBROUTINE

! Procedure 2 : writing data
! --------------------------
SUBROUTINE writeMatrix(field,nbOfRow,nbOfColumn,inputFileUnit)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfColumn,nbOfRow,inputFileUnit
      TYPE(matrixReal4), INTENT(IN) :: field
      REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrField
      
      INTEGER :: k, kk, i1, i2

!     Body
!     - - -
      ptrField => matrixGetValues(field)

      k = 1
      
1     CONTINUE
      kk = k + 7

      IF ( kk <= nbOfColumn ) THEN
         GOTO 3
      ENDIF

      kk = nbOfColumn
      
3     CONTINUE            

      WRITE(inputFileUnit,200)  (i1,i1=k,kk)

      DO i1 = 1, nbOfRow
         WRITE(inputFileUnit,201) i1, (ptrField(i1,i2),i2=k,kk)
      ENDDO

      k = k + 8      

      IF ( k <= nbOfColumn ) THEN
         GOTO 1
      ENDIF

200   FORMAT('0',8(9X,I3,3X))
201   FORMAT(1X,I3,8(E15.5))

END SUBROUTINE

! Procedure 3 : computing sum
! ---------------------------
SUBROUTINE computeSum(fieldA,fieldB,fieldC,nbOfRow,nbOfColumn,exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfColumn,nbOfRow
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      TYPE(matrixReal4), INTENT(IN) :: fieldA, fieldB, fieldC
      REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrFieldA, ptrFieldB, ptrFieldC
      REAL(KIND=4), POINTER :: ptrValueFieldA, ptrValueFieldB
      REAL(KIND=4) :: value, tolerance
      INTEGER :: i1, i2
!     Body
!     - - -
      ptrFieldA => matrixGetValues(fieldA)
      ptrFieldB => matrixGetValues(fieldB)
      ptrFieldC => matrixGetValues(fieldC)

      tolerance = 0.00001*abs(exclusionValue) 

!$OMP PARALLEL DEFAULT(NONE) SHARED(ptrFieldA,ptrFieldB,ptrFieldC,exclusionValue,nbOfColumn,  &
!$OMP                               nbOfRow,tolerance) & 
!$OMP                        PRIVATE(ptrValueFieldA,ptrValueFieldB,value,i1,i2)
!$OMP DO

      DO i2 = 1, nbOfColumn
       DO i1 = 1, nbOfRow
          ptrValueFieldA => ptrFieldA(i1,i2)
          ptrValueFieldB => ptrFieldB(i1,i2)
          value = ptrValueFieldA + ptrValueFieldB
          
          IF ( abs(ptrValueFieldA - exclusionValue) < tolerance ) THEN
             value = exclusionValue
          ENDIF
         
          IF ( abs(ptrValueFieldB - exclusionValue) < tolerance ) THEN
             value = exclusionValue
          ENDIF

          ptrFieldC(i1,i2) = value

       ENDDO
      ENDDO
     
!$OMP END DO
!$OMP END PARALLEL      
      
END SUBROUTINE

END PROGRAM sumgrid
      
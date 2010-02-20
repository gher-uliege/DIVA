PROGRAM filterCV

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE matrixInterface

  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: nbOfSample, nbOfLayer, inputFileUnit, outputFileUnit, i1, i2, i3, jp, jm

   TYPE(file) :: inputFile, outputFile
   TYPE(matrixReal4) :: cv, cvf, w, w1, w2, w3, w4, w5
   REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrCV, ptrCVF, ptrW, ptrW1, ptrW2, ptrW3, ptrW4, ptrW5
   REAL(KIND=4), POINTER :: ptrCVi_j,ptrCVi_jp,ptrCVi_jm, ptrWi_j,ptrWi_jp,ptrWi_jm
   REAL(KIND=4) :: coeffA, coeffB

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

!     1) Read information
!     -------------------
!       1.1) In interactive mode
!       - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the number of samples'
   READ(stdInput,*) nbOfSample
   WRITE(stdOutput,*) 'Please enter the number of layers'
   READ(stdInput,*) nbOfLayer

!       1.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfSample,nbOfLayer
30 CONTINUE

#endif

   PRINT*, 'CV',nbOfSample,nbOfLayer

!     2) Creation of matrix
!     ---------------------
   CALL matrixCreate(cv,nbOfSample,nbOfLayer)
   ptrCV => matrixGetValues(cv)
   CALL matrixCreate(cvf,nbOfSample,nbOfLayer)
   ptrCVF => matrixGetValues(cvf)
   CALL matrixCreate(w,nbOfSample,nbOfLayer)
   ptrW => matrixGetValues(w)
   CALL matrixCreate(w1,nbOfSample,nbOfLayer)
   ptrW1 => matrixGetValues(w1)
   CALL matrixCreate(w2,nbOfSample,nbOfLayer)
   ptrW2 => matrixGetValues(w2)
   CALL matrixCreate(w3,nbOfSample,nbOfLayer)
   ptrW3 => matrixGetValues(w3)
   CALL matrixCreate(w4,nbOfSample,nbOfLayer)
   ptrW4 => matrixGetValues(w4)
   CALL matrixCreate(w5,nbOfSample,nbOfLayer)
   ptrW5 => matrixGetValues(w5)

!     3) Reading data
!     ---------------
   CALL createFile(inputFile,'fort.20',formType=STD_FORMATTED)
   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile) 
   
   DO i2 = 1, nbOfLayer
    DO i1 = 1, nbOfSample
     READ(inputFileUnit,*,END=99,ERR=99) ptrW1(i1,i2),ptrCV(i1,i2),ptrW2(i1,i2),ptrW3(i1,i2),ptrW4(i1,i2), &
                                         ptrW5(i1,i2),ptrW(i1,i2)
    ENDDO
   ENDDO
         
   CALL closeFile(inputFile)
         
!     4) Computing data
!     -----------------
   DO i3 = 1, 3
   
    DO i2 = 1, nbOfLayer
     DO i1 = 1, nbOfSample
      jp = min(i2+1,nbOfLayer)
      jm = max(i2-1,1)
      
      ptrWi_jp => ptrW(i1,jp)
      ptrWi_j => ptrW(i1,i2)
      ptrWi_jm => ptrW(i1,jm)
      
      coeffA = sqrt(ptrWi_jp/(ptrWi_j+ptrWi_jp))
      coeffB = sqrt(ptrWi_jm/(ptrWi_j+ptrWi_jm))
      
      ptrCVi_jp => ptrCV(i1,jp)
      ptrCVi_j => ptrCV(i1,i2)
      ptrCVi_jm => ptrCV(i1,jm)

      ptrCVF(i1,i2) = ptrCVi_j + 0.25 * ( coeffA * ( ptrCVi_jp - ptrCVi_j ) - coeffB * ( ptrCVi_j - ptrCVi_jm ) )
      
     ENDDO
    ENDDO
     
      ptrCV(1:nbOfSample,1:nbOfLayer) = ptrCVF(1:nbOfSample,1:nbOfLayer)
   ENDDO
         
!     5) Writing data
!     ---------------
   CALL createFile(outputFile,'fort.21',formType=STD_FORMATTED)
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile) 
   
   DO i2 = 1, nbOfLayer
    DO i1 = 1, nbOfSample
     WRITE(outputFileUnit,60) ptrW1(i1,i2),ptrCVF(i1,i2),ptrW2(i1,i2),ptrW3(i1,i2),ptrW4(i1,i2), &
                                         ptrW5(i1,i2),ptrW(i1,i2)
    ENDDO
   ENDDO

60 FORMAT(7(E12.5))

   CALL closeFile(outputFile)

   GOTO 100         

!  Always finalise the DIVA context
!  ================================
99 CONTINUE
   PRINT*, 'Invalid input file'
100 CONTINUE

   CALL matrixDestroy(cv)      
   CALL matrixDestroy(cvf)      
   CALL matrixDestroy(w)      
   CALL matrixDestroy(w1)      
   CALL matrixDestroy(w2)      
   CALL matrixDestroy(w3)      
   CALL matrixDestroy(w4)      
   CALL matrixDestroy(w5)      
   CALL finaliseDIVAContext()
   
END PROGRAM filterCV


        

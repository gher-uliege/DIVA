PROGRAM gherValex

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE matrixInterface

  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   REAL(KIND=4) :: exclusionValue
   INTEGER :: nbOfRow, nbOfColumn

   Type(file) :: outputFile
   TYPE(matrixReal4) :: field

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
   WRITE(stdOutput,*) 'Please enter the number of row of the matrix'
   READ(stdInput,*) nbOfRow
   WRITE(stdOutput,*) 'Please enter the number of column of the matrix'
   READ(stdInput,*) nbOfColumn
   WRITE(stdOutput,*) 'Please enter the exclusion value'
   READ(stdInput,*) exclusionValue

!       1.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfRow,nbOfColumn,exclusionValue
30 CONTINUE

#endif

!     2) Creation of matrix
!     ---------------------
   CALL matrixCreate(field,nbOfRow,nbOfColumn)
   CALL matrixSetToValue(field,exclusionValue)

!     3) Creation of needed files
!     ---------------------------
   CALL createFile(outputFile,'fort.84',formType=GHER_UNFORMATTED)
   
   CALL matrixWrite(field,outputFile,exclusionValue)
         
!  Always finalise the DIVA context
!  ================================
   CALL matrixDestroy(field)      
   CALL finaliseDIVAContext()
   
END PROGRAM gherValex


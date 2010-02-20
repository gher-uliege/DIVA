PROGRAM convertGHERFile

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  
  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   REAL(KIND=4) :: readExclusionValue
   INTEGER :: readDimI, readDimJ, readDimK, nbOfWord, iprecision, iCheck
   INTEGER, PARAMETER :: defaultDim = 10000

   Type(file) :: inputFile
   Type(file) :: outputFile
   REAL(KIND=4), DIMENSION(defaultDim) :: vector4
   REAL(KIND=8), DIMENSION(defaultDim) :: vector8
   CHARACTER(LEN=132) :: fileName

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

!     Read name of the file
!     ---------------------
!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the name of the file to convert'
   READ(stdInput,*) fileName
   WRITE(stdOutput,*) 'Please enter the direction : (1) from Formatted to Unformatted, (2) opposite'
   READ(stdInput,*) iCheck

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) fileName,iCheck
30 CONTINUE

#endif

   SELECT CASE (iCheck)
      CASE (1)
        CALL createFile(inputFile,fileName,formType=GHER_FORMATTED)
        CALL ureadcNew(inputFile,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)

        CALL createFile(outputFile,fileName,formType=GHER_UNFORMATTED)
        CALL uwritcNew(outputFile,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
      CASE (2)
        CALL createFile(inputFile,fileName,formType=GHER_UNFORMATTED)
        CALL ureadcNew(inputFile,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)

        CALL createFile(outputFile,fileName,formType=GHER_FORMATTED)
        CALL uwritcNew(outputFile,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   END SELECT
   

         
!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

   
END PROGRAM convertGHERFile

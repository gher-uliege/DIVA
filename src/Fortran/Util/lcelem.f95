PROGRAM lcelem

! Module
! ======
  USE moduleDIVA
  USE moduleFile

! Include file
! ============
   include 'constantParameter.h'
   include 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: fileUnit
   INTEGERType :: isDensValue
   REALType    :: lengthScale
   TYPE(file) :: outputFile

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

!     Read information about the length scale and penalisation for the gradients
!     --------------------------------------------------------------------------
!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the ISDENS value'
   READ(stdInput,*) isDensValue
   WRITE(stdOutput,*) 'Please enter the correlation length of your analysis'
   READ(stdInput,*) lengthScale

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) isDensValue,lengthScale
30 CONTINUE

#endif

!     Write results in file
!     ----------------------
   CALL createFile(outputFile,'fort.11',getLogicalUnit())
   CALL openFile(outputFile)
   fileUnit = getFileUnit(outputFile)

   WRITE(fileUnit,*) isDensValue
   WRITE(fileUnit,*) lengthScale / five

   CALL closeFile(outputFile)

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM  lcelem

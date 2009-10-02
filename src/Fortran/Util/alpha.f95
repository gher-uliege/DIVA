PROGRAM Alpha0_Alpha1

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
   REALType    :: gradientPenalisation, lengthScale
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
   WRITE(stdOutput,*) 'Please enter the correlation length of your analysis'
   READ(stdInput,*) lengthScale
   WRITE(stdOutput,*) 'Please enter the penalisation on the gradients'
   READ(stdInput,*) gradientPenalisation

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*,END=30) lengthScale,gradientPenalisation
30 CONTINUE

#endif

!     Write results in file
!     ----------------------
   CALL createFile(outputFile,'fort.12',getLogicalUnit())
   CALL openFile(outputFile)
   fileUnit = getFileUnit(outputFile)

   WRITE(fileUnit,*) one / lengthScale**four
   WRITE(fileUnit,*) two * gradientPenalisation / lengthScale**two
   
   CALL closeFile(outputFile)

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM  Alpha0_Alpha1

PROGRAM multiply

! Include file
! ============
   INCLUDE 'ioParameter.h'

! Declaration
! ===========
   REALType   :: value1, value2

! ==================
! ==================
! == Main program ==
! ==================
! ==================

!  Body
!  ====
#ifdef _BATCH_MODE_
#undef _INTERACTIVE_MODE_
#endif

!     Read information
!     ----------------

!     1) In interactive mode
!     - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Enter the first value'
   READ(stdInput,*) value1
   WRITE(stdOutput,*) 'Enter the second value'
   READ(stdInput,*) value2

!     2) In batch mode
!     - - - - - - - - -
#else
   READ(stdInput,*) value1,value2

#endif

!     Write result
!     ------------
   WRITE(stdOutput,*) ' value1 * value2 = ', value1*value2

END PROGRAM  multiply

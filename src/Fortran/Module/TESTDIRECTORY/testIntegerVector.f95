PROGRAM testVector

! =====================================================
! =====================================================
! ===                                               ===
! ===   This program is testing the vector module   ===
! ===                                               ===
! =====================================================
! =====================================================

! Module to use
! =============
 USE moduleDIVA
 USE vectorInterface

! Declaration
! ===========
TYPE(vectorType) :: vector1

INTEGER, PARAMETER :: dim = 3

! Main program
! ============
    CALL createDIVAContext()

!   1) testing the creation procedure
!   ---------------------------------

  PRINT*,'checkAllocationMemoryProcedure'
  PRINT*,'=============================='
  CALL checkAllocationMemoryProcedure()
  PRINT*,' '

!    1.1) Basic creation procedure
!    - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1)
  CALL vectorSetSize(vector1,dim)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.2) creation procedure with dimension
!    - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.3) creation procedure with dimension and starting point
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim,-1)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.4) Basic creation procedure with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - -

  CALL vectorCreate(vector1)
  CALL vectorSetSize(vector1,dim)
  CALL vectorOptimize(vector1)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim)
  CALL vectorOptimize(vector1)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim,-1)
  CALL vectorOptimize(vector1)
  CALL vectorSetIncreaseSize(vector1,0)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

! End program
! ===========
    CALL finaliseDIVAContext()

 CONTAINS

! ==============================================
! ==============================================
! ==                                          ==
! ==                                          ==
! ==            Internal procedure            ==
! ==                                          ==
! ==                                          ==
! ==============================================
! ==============================================

 SUBROUTINE checkProcedure(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
  PRINT*,'checkVectorSetToZero'
  PRINT*,'===================='
  CALL checkVectorSetToZero(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSetToValue'
  PRINT*,'====================='
  CALL checkVectorSetToValue(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSetInsertValue'
  PRINT*,'========================='
  CALL checkVectorSetInsertValue(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSetAddValue'
  PRINT*,'======================'
  CALL checkVectorSetAddValue(vector1)
  PRINT*,' '
  PRINT*,'checkVectorMin'
  PRINT*,'=============='
  CALL checkVectorMin(vector1)
  PRINT*,' '
  PRINT*,'checkVectorMax'
  PRINT*,'=============='
  CALL checkVectorMax(vector1)
  PRINT*,' '
  PRINT*,'checkVectorAbsMin'
  PRINT*,'================='
  CALL checkVectorAbsMin(vector1)
  PRINT*,' '
  PRINT*,'checkVectorAbsMax'
  PRINT*,'================='
  CALL checkVectorAbsMax(vector1)
  PRINT*,' '
  PRINT*,'checkVectorGetValues'
  PRINT*,'===================='
  CALL checkVectorGetValues(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSetSize'
  PRINT*,'=================='
  CALL checkVectorSetSize(vector1)
  PRINT*,' '


 END SUBROUTINE

 SUBROUTINE checkVectorSetToZero(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
   CALL vectorSetToZero(vector1)
   CALL vectorPrint(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetToValue(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1
   VARType, PARAMETER :: val = 25

!  Body
!  - - -
   CALL vectorSetToValue(vector1,val)
   CALL vectorPrint(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetInsertValue(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1
   VARType, PARAMETER :: val = 15

!  Body
!  - - -
   CALL vectorInsertValue(vector1,1,val)
   CALL vectorPrint(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetAddValue(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1
   VARType, PARAMETER :: val = 45

!  Body
!  - - -
   CALL vectorAddValue(vector1,1,val)
   CALL vectorPrint(vector1)

 END SUBROUTINE


 SUBROUTINE checkVectorMin(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector min : ', vectorMin(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorAbsMax(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector max : ', vectorAbsMax(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorAbsMin(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector abs min : ', vectorAbsMin(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorMax(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector abs max : ', vectorMax(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorGetValues(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1
   VARType, DIMENSION(:), POINTER :: ptr
   INTEGER :: istart, iend, i1

!  Body
!  - - -
  ptr => vectorGetValues(vector1)
  istart = vectorGetFirstIndex(vector1)
  iend = vectorGetLastIndex(vector1)
  DO i1 = istart, iend
     ptr(i1) = 23.
  ENDDO

  CALL vectorPrint(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetSize(vector1)

!  Declaration
!  -----------
   TYPE(vectorType), INTENT(INOUT) :: vector1
   INTEGER :: istart, iend, i1
   VARType, PARAMETER :: val = 0
   VARType, PARAMETER :: val1 = 1
   VARType, PARAMETER :: val11 = 1
   VARType, PARAMETER :: val2 = -3
   VARType, PARAMETER :: val3 = 4

!  Body
!  - - -
  istart = vectorGetFirstIndex(vector1)
  iend = vectorGetLastIndex(vector1)

   DO i1 = istart, iend
       CALL vectorSetValue(vector1,i1,val,FAST_INSERT_VALUE)
       CALL vectorSetValue(vector1,i1,val1,FAST_ADD_VALUE)
       CALL vectorSetValue(vector1,i1,val11,ADD_VALUE)
   ENDDO

   CALL vectorPrint(vector1)

   DO i1 = istart - 2 , istart - 1
       CALL vectorSetValue(vector1,i1,val2,INSERT_VALUE)
   ENDDO

   DO i1 = iend + 1  , iend + 2
       CALL vectorSetValue(vector1,i1,val3,INSERT_VALUE)
   ENDDO

   CALL vectorPrint(vector1)

 END SUBROUTINE

 SUBROUTINE checkAllocationMemoryProcedure()

!  Declaration
!  -----------
   TYPE(vectorType) :: vector1

!  Body
!  - - -
   CALL vectorCreate(vector1)
   CALL vectorSetSize(vector1,3)
   CALL vectorSetToZero(vector1)
   CALL vectorPrint(vector1)
   CALL vectorSetIncreaseSize(vector1,0)
   CALL vectorSetSize(vector1,9)
   CALL vectorSetToZero(vector1)
   CALL vectorPrint(vector1)
   CALL vectorSetIncreaseSize(vector1,-1)
   CALL vectorSetSize(vector1,10)
   CALL vectorSetToZero(vector1)
   CALL vectorPrint(vector1)
   CALL vectorDestroy(vector1)

 END SUBROUTINE

END PROGRAM testVector


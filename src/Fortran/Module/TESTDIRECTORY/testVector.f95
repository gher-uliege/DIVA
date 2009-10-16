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
 USE moduleVector

! Declaration
! ===========
TYPE(vector) :: vector1

INTEGER, PARAMETER :: dim = 3

! Main program
! ============
    CALL createDIVAContext()

!   1) testing the creation procedure
!   ---------------------------------
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
  CALL vectorSetMemoryIncreaseSize(0)

  CALL vectorCreate(vector1)
  CALL vectorSetSize(vector1,dim)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim)
  CALL checkProcedure(vector1)
  CALL vectorDestroy(vector1)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL vectorCreate(vector1,dim,-1)
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
   TYPE(vector), INTENT(INOUT) :: vector1

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
  PRINT*,'checkVectorNorm'
  PRINT*,'==============='
  CALL checkVectorNorm(vector1)
  PRINT*,' '
  PRINT*,'checkVectorScale'
  PRINT*,'================'
  CALL checkVectorScale(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSqrt'
  PRINT*,'==============='
  CALL checkVectorSqrt(vector1)
  PRINT*,' '
  PRINT*,'checkVectorSum'
  PRINT*,'=============='
  CALL checkVectorSum(vector1)
  PRINT*,' '
  PRINT*,'checkVectorMin'
  PRINT*,'=============='
  CALL checkVectorMin(vector1)
  PRINT*,' '
  PRINT*,'checkVectorMax'
  PRINT*,'=============='
  CALL checkVectorMax(vector1)
  PRINT*,' '
  PRINT*,'checkVectorDot'
  PRINT*,'=============='
  CALL checkVectorDot(vector1)
  PRINT*,' '


 END SUBROUTINE
 
 SUBROUTINE checkVectorSetToZero(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   
!  Body
!  - - -
   CALL vectorSetToZero(vector1)
   CALL printInformation(vector1)
   
 END SUBROUTINE

 SUBROUTINE checkVectorSetToValue(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   REALType, PARAMETER :: val = 25.D+0

!  Body
!  - - -
   CALL vectorSetToValue(vector1,val)
   CALL printInformation(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetInsertValue(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   REALType, PARAMETER :: val = 15.D+0

!  Body
!  - - -
   CALL vectorInsertValue(vector1,1,val)
   CALL printInformation(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSetAddValue(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   REALType, PARAMETER :: val = 45.D+0

!  Body
!  - - -
   CALL vectorAddValue(vector1,1,val)
   CALL printInformation(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorNorm(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Norm L1 : ', vectorNorm1(vector1)
   PRINT*,'Norm L2 : ', vectorNorm2(vector1)
   PRINT*,'Norm Inf : ', vectorNormInfinity(vector1)
   PRINT*,'Norm L1 : ', vectorNorm(vector1,normL1)
   PRINT*,'Norm L2 : ', vectorNorm(vector1,normL2)
   PRINT*,'Norm Inf : ', vectorNorm(vector1,normInfinity)

 END SUBROUTINE

 SUBROUTINE checkVectorScale(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   REALType, PARAMETER :: val = 0.32D+0

!  Body
!  - - -
   CALL vectorScale(vector1,val)
   CALL printInformation(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSqrt(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1

!  Body
!  - - -
   CALL vectorSqrt(vector1)
   CALL printInformation(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorSum(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector sum : ', vectorSum(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorMin(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector min : ', vectorMin(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorMax(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1

!  Body
!  - - -
   PRINT*,'Vector max : ', vectorMax(vector1)

 END SUBROUTINE

 SUBROUTINE checkVectorDot(vector1)

!  Declaration
!  -----------
   TYPE(vector), INTENT(INOUT) :: vector1
   TYPE(vector) :: vector2

!  Body
!  - - -
  CALL vectorCreate(vector2)
  CALL vectorSetSize(vector2,vectorGetSize(vector1))
  CALL vectorSetToValue(vector2,2.3D+0)
  CALL printInformation(vector2)
  PRINT*,'Dot product : ', vectorDot(vector1,vector2)
  CALL vectorDestroy(vector2)
  
 END SUBROUTINE

END PROGRAM testVector


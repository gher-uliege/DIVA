PROGRAM testArray

! =====================================================
! =====================================================
! ===                                               ===
! ===   This program is testing the array module   ===
! ===                                               ===
! =====================================================
! =====================================================

! Module to use
! =============
 USE moduleDIVA
 USE moduleNorm
 USE matrixInterface

! Declaration
! ===========
TYPE(arrayType) :: array1

INTEGER, PARAMETER :: dimX = 3
INTEGER, PARAMETER :: dimY = 3

! Main program
! ============
    CALL createDIVAContext()

!   1) testing the creation procedure
!   ---------------------------------
  CALL arraySetDefaultMemoryIncreaseSize(5,5)
  
  PRINT*,'checkAllocationMemoryProcedure'
  PRINT*,'=============================='
  CALL checkAllocationMemoryProcedure()
  PRINT*,' '

!    1.1) Basic creation procedure
!    - - - - - - - - - - - - - - -
  CALL matrixCreate(array1)
  CALL matrixSetSize(array1,dimX,dimY)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)
  

!    1.2) creation procedure with dimension
!    - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.3) creation procedure with dimension and starting point
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY,-1,-1)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.4) Basic creation procedure with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - -
  CALL arraySetDefaultMemoryIncreaseSize(0,0)

  CALL matrixCreate(array1)
  CALL matrixSetSize(array1,dimX,dimY)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY,-1,-1)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

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

 SUBROUTINE checkProcedure(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
  PRINT*,'checkArraySetToZero'
  PRINT*,'===================='
  CALL checkArraySetToZero(array1)
  PRINT*,' '
  PRINT*,'checkArraySetToValue'
  PRINT*,'====================='
  CALL checkArraySetToValue(array1)
  PRINT*,' '
  PRINT*,'checkArraySetInsertValue'
  PRINT*,'========================='
  CALL checkArraySetInsertValue(array1)
  PRINT*,' '
  PRINT*,'checkArraySetAddValue'
  PRINT*,'======================'
  CALL checkArraySetAddValue(array1)
  PRINT*,' '
  PRINT*,'checkArrayNorm'
  PRINT*,'==============='
  CALL checkArrayNorm(array1)
  PRINT*,' '
  PRINT*,'checkArrayScale'
  PRINT*,'================'
  CALL checkArrayScale(array1)
  PRINT*,' '
  PRINT*,'checkArraySqrt'
  PRINT*,'==============='
  CALL checkArraySqrt(array1)
  PRINT*,' '
  PRINT*,'checkArraySum'
  PRINT*,'=============='
  CALL checkArraySum(array1)
  PRINT*,' '
  PRINT*,'checkArrayMin'
  PRINT*,'=============='
  CALL checkArrayMin(array1)
  PRINT*,' '
  PRINT*,'checkArrayMax'
  PRINT*,'=============='
  CALL checkArrayMax(array1)
  PRINT*,' '
  PRINT*,'checkArrayDot'
  PRINT*,'=============='
  CALL checkArrayDot(array1)
  PRINT*,' '
  PRINT*,'checkArrayGetValues'
  PRINT*,'===================='
  CALL checkArrayGetValues(array1)
  PRINT*,' '
  PRINT*,'checkArraySetSize'
  PRINT*,'=================='
  CALL checkArraySetSize(array1)
  PRINT*,' '


 END SUBROUTINE
 
 SUBROUTINE checkArraySetToZero(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   
!  Body
!  - - -
   CALL matrixSetToZero(array1)
   CALL printInformation(array1)
   
 END SUBROUTINE

 SUBROUTINE checkArraySetToValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 25.

!  Body
!  - - -
   CALL matrixSetToValue(array1,val)
   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetInsertValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 15.

!  Body
!  - - -
   CALL matrixInsertValue(array1,1,1,val)
   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetAddValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 45.

!  Body
!  - - -
   CALL matrixAddValue(array1,1,1,val)
   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayNorm(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'Norm L1 : ', matrixNorm1(array1)
   PRINT*,'Norm L2 : ', matrixNorm2(array1)
   PRINT*,'Norm Inf : ', matrixNormInfinity(array1)
   PRINT*,'Norm L1 : ', matrixNorm(array1,normL1)
   PRINT*,'Norm L2 : ', matrixNorm(array1,normL2)
   PRINT*,'Norm Inf : ', matrixNorm(array1,normInfinity)

 END SUBROUTINE

 SUBROUTINE checkArrayScale(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 0.32

!  Body
!  - - -
   CALL matrixScale(array1,val)
   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySqrt(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   CALL matrixSqrt(array1)
   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySum(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array sum : ',matrixSum(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayMin(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array min : ', matrixMin(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayMax(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array max : ', matrixMax(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayGetValues(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, DIMENSION(:,:), POINTER :: ptr
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2

!  Body
!  - - -
  ptr => matrixGetValues(array1)
  istartX = matrixGetStartIndexX(array1)
  iendX = matrixGetEndIndexX(array1,istartX)
  istartY = matrixGetStartIndexY(array1)
  iendY = matrixGetEndIndexY(array1,istartY)

  DO i1 = istartX, iendX
   DO i2 = istartY, iendY
     ptr(i1,i2) = 23.
   ENDDO
  ENDDO

  CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetSize(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, DIMENSION(:,:), POINTER :: ptr
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2
   VARType, PARAMETER :: val = 2.3
   VARType, PARAMETER :: val2 = -3.3
   VARType, PARAMETER :: val3 = 3.3
   VARType, PARAMETER :: val4 = 4.3
   VARType, PARAMETER :: val5 = 5.3

!  Body
!  - - -
  istartX = matrixGetStartIndexX(array1)
  iendX = matrixGetEndIndexX(array1,istartX)
  istartY = matrixGetStartIndexY(array1)
  iendY = matrixGetEndIndexY(array1,istartY)

   CALL matrixSetToValue(array1,val)
   CALL printInformation(array1)

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY - 2 , istartY - 1
       CALL matrixInsertValue(array1,i1,i2,val2)
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = iendY + 1 , iendY + 2
       CALL matrixInsertValue(array1,i1,i2,val3)
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY - 2 , istartY - 1
       CALL matrixInsertValue(array1,i1,i2,val4)
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = iendY + 1 , iendY + 2
       CALL matrixInsertValue(array1,i1,i2,val5)
    ENDDO
   ENDDO

   CALL printInformation(array1)

 END SUBROUTINE

 SUBROUTINE checkAllocationMemoryProcedure()
 
!  Declaration
!  -----------
   TYPE(arrayType) :: array1

!  Body
!  - - -
   CALL arrayCreate(array1)
   CALL arraySetSize(array1,3,3)
   CALL arraySetToZero(array1)
   CALL printInformation(array1)
   CALL arraySetIncreaseSize(array1,0,0)
   CALL arraySetSize(array1,9,9)
   CALL arraySetToZero(array1)
   CALL printInformation(array1)
   CALL arraySetIncreaseSize(array1,-1,-1)
   CALL arraySetSize(array1,10,10)
   CALL arraySetToZero(array1)
   CALL printInformation(array1)
   CALL arrayDestroy(array1)

 END SUBROUTINE

END PROGRAM testArray


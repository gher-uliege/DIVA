PROGRAM testMatrix

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
 USE matrixInterface

! Declaration
! ===========
TYPE(matrixType) :: array1

INTEGER, PARAMETER :: dimX = 3
INTEGER, PARAMETER :: dimY = 3

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

  CALL matrixCreate(array1)
  CALL matrixSetSize(array1,dimX,dimY)
  CALL matrixOptimize(array1)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY)
  CALL matrixOptimize(array1)
  CALL checkProcedure(array1)
  CALL matrixDestroy(array1)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL matrixCreate(array1,dimX,dimY,-1,-1)
  CALL matrixOptimize(array1)
  CALL matrixSetIncreaseSize(array1,0,0)
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
   TYPE(matrixType), INTENT(INOUT) :: array1

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
  PRINT*,'checkArrayMin'
  PRINT*,'=============='
  CALL checkArrayMin(array1)
  PRINT*,' '
  PRINT*,'checkArrayMax'
  PRINT*,'=============='
  CALL checkArrayMax(array1)
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
   TYPE(matrixType), INTENT(INOUT) :: array1
   
!  Body
!  - - -
   CALL matrixSetToZero(array1)
   CALL matrixPrint(array1)
   
 END SUBROUTINE

 SUBROUTINE checkArraySetToValue(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 25

!  Body
!  - - -
   CALL matrixSetToValue(array1,val)
   CALL matrixPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetInsertValue(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 15

!  Body
!  - - -
   CALL matrixInsertValue(array1,1,1,val)
   CALL matrixPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetAddValue(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 45

!  Body
!  - - -
   CALL matrixAddValue(array1,1,1,val)
   CALL matrixPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayMin(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array min : ', matrixMin(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayMax(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array max : ', matrixMax(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayGetValues(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1
   VARType, DIMENSION(:,:), POINTER :: ptr
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2

!  Body
!  - - -
  ptr => matrixGetValues(array1)
  istartX = matrixGetFirstIndexX(array1)
  iendX = matrixGetLastIndexX(array1)
  istartY = matrixGetFirstIndexY(array1)
  iendY = matrixGetLastIndexY(array1)

  DO i1 = istartX, iendX
   DO i2 = istartY, iendY
     ptr(i1,i2) = 23
   ENDDO
  ENDDO

  CALL matrixPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetSize(array1)

!  Declaration
!  -----------
   TYPE(matrixType), INTENT(INOUT) :: array1
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2
   VARType, PARAMETER :: val001 = 0
   VARType, PARAMETER :: val002 = 1
   VARType, PARAMETER :: val003 = 1
   VARType, PARAMETER :: val2 = -3
   VARType, PARAMETER :: val3 = 4
   VARType, PARAMETER :: val4 = 5
   VARType, PARAMETER :: val5 = 6
   VARType, PARAMETER :: val6 = 7
   VARType, PARAMETER :: val7 = 8
   VARType, PARAMETER :: val8 = 9
   VARType, PARAMETER :: val9 = 10

!  Body
!  - - -
  istartX = matrixGetFirstIndexX(array1)
  iendX = matrixGetLastIndexX(array1)
  istartY = matrixGetFirstIndexY(array1)
  iendY = matrixGetLastIndexY(array1)

   DO i1 = istartX , iendX
    DO i2 = istartY , iendY
       CALL matrixSetValue(array1,i1,i2,val001,FAST_INSERT_VALUE)
       CALL matrixSetValue(array1,i1,i2,val002,FAST_ADD_VALUE)
       CALL matrixSetValue(array1,i1,i2,val003,ADD_VALUE)
    ENDDO
   ENDDO

   CALL matrixPrint(array1)

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY - 2 , istartY - 1
       CALL matrixSetValue(array1,i1,i2,val2,INSERT_VALUE)
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = iendY + 1 , iendY + 2
       CALL matrixInsertValue(array1,i1,i2,val3)
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY , iendY
       CALL matrixInsertValue(array1,i1,i2,val6)
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

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY , iendY
       CALL matrixInsertValue(array1,i1,i2,val7)
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY - 2 , istartY - 1
       CALL matrixInsertValue(array1,i1,i2,val8)
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = iendY + 1 , iendY + 2
       CALL matrixInsertValue(array1,i1,i2,val9)
    ENDDO
   ENDDO

   CALL matrixPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkAllocationMemoryProcedure()

!  Declaration
!  -----------
   TYPE(matrixType) :: matrix1

!  Body
!  - - -
   CALL matrixCreate(matrix1)
   CALL matrixSetSize(matrix1,3,3)
   CALL matrixSetToZero(matrix1)
   CALL matrixPrint(matrix1)
   CALL matrixSetIncreaseSize(matrix1,0,0)
   CALL matrixSetSize(matrix1,9,9)
   CALL matrixSetToZero(matrix1)
   CALL matrixPrint(matrix1)
   CALL matrixSetIncreaseSize(matrix1,-1,-1)
   CALL matrixSetSize(matrix1,10,10)
   CALL matrixSetToZero(matrix1)
   CALL matrixPrint(matrix1)
   CALL matrixDestroy(matrix1)

 END SUBROUTINE

END PROGRAM testMatrix


PROGRAM testArray3D

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
 USE array3DInterface

! Declaration
! ===========
TYPE(arrayType) :: array1

INTEGER, PARAMETER :: dimX = 3
INTEGER, PARAMETER :: dimY = 3
INTEGER, PARAMETER :: dimZ = 3

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
  CALL arrayCreate(array1)
  CALL arraySetSize(array1,dimX,dimY,dimZ)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)
  

!    1.2) creation procedure with dimension
!    - - - - - - - - - - - - - - - - - - - -
  CALL arrayCreate(array1,dimX,dimY,dimZ)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)

!    1.3) creation procedure with dimension and starting point
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL arrayCreate(array1,dimX,dimY,dimZ,-1,-1,-1)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)

!    1.4) Basic creation procedure with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - -

  CALL arrayCreate(array1)
  CALL arraySetSize(array1,dimX,dimY,dimZ)
  CALL arrayOptimize(array1)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL arrayCreate(array1,dimX,dimY,dimZ)
  CALL arrayOptimize(array1)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL arrayCreate(array1,dimX,dimY,dimZ,-1,-1,-1)
  CALL arrayOptimize(array1)
  CALL arraySetIncreaseSize(array1,0,0,0)
  CALL checkProcedure(array1)
  CALL arrayDestroy(array1)

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
   TYPE(arrayType), INTENT(INOUT) :: array1
   
!  Body
!  - - -
   CALL arraySetToZero(array1)
   CALL arrayPrint(array1)
   
 END SUBROUTINE

 SUBROUTINE checkArraySetToValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 25

!  Body
!  - - -
   CALL arraySetToValue(array1,val)
   CALL arrayPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetInsertValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 15

!  Body
!  - - -
   CALL arrayInsertValue(array1,1,1,1,val)
   CALL arrayPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetAddValue(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, PARAMETER :: val = 45

!  Body
!  - - -
   CALL arrayAddValue(array1,1,1,1,val)
   CALL arrayPrint(array1)

 END SUBROUTINE


 SUBROUTINE checkArrayMin(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array min : ', arrayMin(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayMax(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1

!  Body
!  - - -
   PRINT*,'array max : ', arrayMax(array1)

 END SUBROUTINE

 SUBROUTINE checkArrayGetValues(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   VARType, DIMENSION(:,:,:), POINTER :: ptr
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2
   INTEGER :: istartZ, iendZ, i3

!  Body
!  - - -
  ptr => arrayGetValues(array1)
  istartX = arrayGetFirstIndexX(array1)
  iendX = arrayGetLastIndexX(array1)
  istartY = arrayGetFirstIndexY(array1)
  iendY = arrayGetLastIndexY(array1)
  istartZ = arrayGetFirstIndexZ(array1)
  iendZ = arrayGetLastIndexZ(array1)

  DO i1 = istartX, iendX
   DO i2 = istartY, iendY
    DO i3 = istartZ, iendZ
     ptr(i1,i2,i3) = 23
    ENDDO
   ENDDO
  ENDDO

  CALL arrayPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkArraySetSize(array1)

!  Declaration
!  -----------
   TYPE(arrayType), INTENT(INOUT) :: array1
   INTEGER :: istartX, iendX, i1
   INTEGER :: istartY, iendY, i2
   INTEGER :: istartZ, iendZ, i3
   VARType, PARAMETER :: val001 = 10
   VARType, PARAMETER :: val002 = 10
   VARType, PARAMETER :: val003 = 3
   VARType, PARAMETER :: val2 = -33
   VARType, PARAMETER :: val3 = 43
   VARType, PARAMETER :: val4 = 53
   VARType, PARAMETER :: val5 = 63
   VARType, PARAMETER :: val6 = 73
   VARType, PARAMETER :: val7 = 83
   VARType, PARAMETER :: val8 = 93
   VARType, PARAMETER :: val9 = 103
   VARType, PARAMETER :: val10 = 113
   VARType, PARAMETER :: val11 = 123
   VARType, PARAMETER :: val12 = -133
   VARType, PARAMETER :: val13 = 143
   VARType, PARAMETER :: val14 = 153
   VARType, PARAMETER :: val15 = 163
   VARType, PARAMETER :: val16 = 173
   VARType, PARAMETER :: val17 = 183
   VARType, PARAMETER :: val18 = 193
   VARType, PARAMETER :: val19 = 203
   VARType, PARAMETER :: val20 = 2113
   VARType, PARAMETER :: val21 = 223
   VARType, PARAMETER :: val22 = -233
   VARType, PARAMETER :: val23 = 243
   VARType, PARAMETER :: val24 = 253
   VARType, PARAMETER :: val25 = 263
   VARType, PARAMETER :: val26 = 273
   VARType, PARAMETER :: val27 = 283
   VARType, PARAMETER :: val28 = 293

!  Body
!  - - -
  istartX = arrayGetFirstIndexX(array1)
  iendX = arrayGetLastIndexX(array1)
  istartY = arrayGetFirstIndexY(array1)
  iendY = arrayGetLastIndexY(array1)
  istartZ = arrayGetFirstIndexZ(array1)
  iendZ = arrayGetLastIndexZ(array1)

   DO i1 = istartX , iendX
    DO i2 = istartY , iendY
     DO i3 = istartZ , iendZ
       CALL arraySetValue(array1,i1,i2,i3,val001,FAST_INSERT_VALUE)
       CALL arraySetValue(array1,i1,i2,i3,val002,FAST_ADD_VALUE)
       CALL arraySetValue(array1,i1,i2,i3,val003,ADD_VALUE)
     ENDDO
    ENDDO
   ENDDO

   CALL arrayPrint(array1)

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arraySetValue(array1,i1,i2,i3,val2,INSERT_VALUE)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val3)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val4)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY , iendY
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val5)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY , iendY
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val6)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = istartY , iendY
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val7)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val8)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val9)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX - 2 , istartX - 1
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val10)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val11)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val12)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val13)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY , iendY
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val14)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = istartY , iendY
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val16)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val17)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val18)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = istartX , iendX
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val19)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val20)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val21)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY - 2 , istartY - 1
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val22)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY , iendY
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val23)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY , iendY
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val24)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = istartY , iendY
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val25)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ - 2 , istartZ - 1
       CALL arrayInsertValue(array1,i1,i2,i3,val26)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = istartZ , iendZ
       CALL arrayInsertValue(array1,i1,i2,i3,val27)
     ENDDO
    ENDDO
   ENDDO

   DO i1 = iendX + 1 , iendX + 2
    DO i2 = iendY + 1 , iendY + 2
     DO i3 = iendZ + 1 , iendZ + 2
       CALL arrayInsertValue(array1,i1,i2,i3,val28)
     ENDDO
    ENDDO
   ENDDO

   CALL arrayPrint(array1)

 END SUBROUTINE

 SUBROUTINE checkAllocationMemoryProcedure()
 
!  Declaration
!  -----------
   TYPE(arrayType) :: array1

!  Body
!  - - -
   CALL arrayCreate(array1)
   CALL arraySetSize(array1,3,3,3)
   CALL arraySetToZero(array1)
   CALL arrayPrint(array1)
   CALL arraySetIncreaseSize(array1,0,0,0)
   CALL arraySetSize(array1,9,9,9)
   CALL arraySetToZero(array1)
   CALL arrayPrint(array1)
   CALL arraySetIncreaseSize(array1,-1,-1,-1)
   CALL arraySetSize(array1,10,10,10)
   CALL arraySetToZero(array1)
   CALL arrayPrint(array1)
   CALL arrayDestroy(array1)

 END SUBROUTINE

END PROGRAM testArray3D


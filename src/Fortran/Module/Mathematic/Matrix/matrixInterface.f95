MODULE matrixInterface
! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module specifications               ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! Include file
! ============
   USE array2DInteger2Definition
   USE array2DInteger4Definition
   USE array2DInteger8Definition
   USE array2DReal4Definition
   USE array2DReal8Definition

   USE moduleArray2DInteger2, ONLY : array2DprintInformationInteger2 => printInformation,    &
                                 array2DDestroyInteger2    =>  arrayDestroy,      &
                                 array2DSetSizeInteger2    =>  arraySetSize,      &
                                 array2DGetSizeXInteger2    =>  arrayGetSizeX,      &
                                 array2DGetSizeYInteger2    =>  arrayGetSizeY,      &
                                 array2DSetToZeroInteger2  =>  arraySetToZero,    &
                                 array2DSetToValueInteger2 =>  arraySetToValue,   &
                                 array2DMinInteger2        =>  arrayMin,          &
                                 array2DMaxInteger2        =>  arrayMax,          &
                                 array2DInsertValueInteger2   =>  arrayInsertValue,   &
                                 array2DAddValueInteger2   =>  arrayAddValue,     &
                                 array2DGetValueInteger2   =>  arrayGetValue,     &
                                 array2DCreateBaseInteger2 =>  arrayCreateBase,   &
                                 array2DCreateWithDimensionInteger2 =>  arrayCreateWithDimension,   &
                                 array2DCreateWithDimensionAndStartingPointInteger2 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array2DGetValuesInteger2  => arrayGetValues,        &
                                 array2DGetStartIndexXInteger2 => arrayGetStartIndexX, &
                                 array2DGetEndIndexXInteger2 => arrayGetEndIndexX ,&
                                 array2DGetStartIndexYInteger2 => arrayGetStartIndexY, &
                                 array2DGetEndIndexYInteger2 => arrayGetEndIndexY

   USE moduleArray2DInteger4, ONLY : array2DprintInformationInteger4 => printInformation,    &
                                 array2DDestroyInteger4    =>  arrayDestroy,      &
                                 array2DSetSizeInteger4    =>  arraySetSize,      &
                                 array2DGetSizeXInteger4    =>  arrayGetSizeX,      &
                                 array2DGetSizeYInteger4    =>  arrayGetSizeY,      &
                                 array2DSetToZeroInteger4  =>  arraySetToZero,    &
                                 array2DSetToValueInteger4 =>  arraySetToValue,   &
                                 array2DMinInteger4        =>  arrayMin,          &
                                 array2DMaxInteger4        =>  arrayMax,          &
                                 array2DInsertValueInteger4   =>  arrayInsertValue,   &
                                 array2DAddValueInteger4   =>  arrayAddValue,     &
                                 array2DGetValueInteger4   =>  arrayGetValue,     &
                                 array2DCreateBaseInteger4 =>  arrayCreateBase,   &
                                 array2DCreateWithDimensionInteger4 =>  arrayCreateWithDimension,   &
                                 array2DCreateWithDimensionAndStartingPointInteger4 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array2DGetValuesInteger4  => arrayGetValues,        &
                                 array2DGetStartIndexXInteger4 => arrayGetStartIndexX, &
                                 array2DGetEndIndexXInteger4 => arrayGetEndIndexX, &
                                 array2DGetStartIndexYInteger4 => arrayGetStartIndexY, &
                                 array2DGetEndIndexYInteger4 => arrayGetEndIndexY

   USE moduleArray2DInteger8, ONLY : array2DprintInformationInteger8 => printInformation,    &
                                 array2DDestroyInteger8    =>  arrayDestroy,      &
                                 array2DSetSizeInteger8    =>  arraySetSize,      &
                                 array2DGetSizeXInteger8    =>  arrayGetSizeX,      &
                                 array2DGetSizeYInteger8    =>  arrayGetSizeY,      &
                                 array2DSetToZeroInteger8  =>  arraySetToZero,    &
                                 array2DSetToValueInteger8 =>  arraySetToValue,   &
                                 array2DMinInteger8        =>  arrayMin,          &
                                 array2DMaxInteger8        =>  arrayMax,          &
                                 array2DInsertValueInteger8   =>  arrayInsertValue,   &
                                 array2DAddValueInteger8   =>  arrayAddValue,     &
                                 array2DGetValueInteger8   =>  arrayGetValue,     &
                                 array2DCreateBaseInteger8 =>  arrayCreateBase,   &
                                 array2DCreateWithDimensionInteger8 =>  arrayCreateWithDimension,   &
                                 array2DCreateWithDimensionAndStartingPointInteger8 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array2DGetValuesInteger8  => arrayGetValues,        &
                                 array2DGetStartIndexXInteger8 => arrayGetStartIndexX, &
                                 array2DGetEndIndexXInteger8 => arrayGetEndIndexX, &
                                 array2DGetStartIndexYInteger8 => arrayGetStartIndexY, &
                                 array2DGetEndIndexYInteger8 => arrayGetEndIndexY

   USE moduleArray2DReal4, ONLY : array2DprintInformationReal4 => printInformation,    &
                                 array2DDestroyReal4    =>  arrayDestroy,      &
                                 array2DSetSizeReal4    =>  arraySetSize,      &
                                 array2DGetSizeXReal4    =>  arrayGetSizeX,      &
                                 array2DGetSizeYReal4    =>  arrayGetSizeY,      &
                                 array2DSetToZeroReal4  =>  arraySetToZero,    &
                                 array2DSetToValueReal4 =>  arraySetToValue,   &
                                 array2DNorm1Real4      =>  arrayNorm1,        &
                                 array2DNorm2Real4      =>  arrayNorm2,        &
                                 array2DNormInfinityReal4  =>  arrayNormInfinity,  &
                                 array2DSqrtReal4       =>  arraySqrt,         &
                                 array2DSumReal4        =>  arraySum,          &
                                 array2DMinReal4        =>  arrayMin,          &
                                 array2DMaxReal4        =>  arrayMax,          &
                                 array2DInsertValueReal4   =>  arrayInsertValue,   &
                                 array2DAddValueReal4   =>  arrayAddValue,     &
                                 array2DScaleReal4      =>  arrayScale,        &
                                 array2DGetValueReal4   =>  arrayGetValue,     &
                                 array2DNormReal4       =>  arrayNorm,         &
                                 array2DCreateBaseReal4 =>  arrayCreateBase,   &
                                 array2DCreateWithDimensionReal4 =>  arrayCreateWithDimension,   &
                                 array2DCreateWithDimensionAndStartingPointReal4 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array2DGetValuesReal4  => arrayGetValues,        &
                                 array2DGetStartIndexXReal4 => arrayGetStartIndexX, &
                                 array2DGetEndIndexXReal4 => arrayGetEndIndexX,    &
                                 array2DGetStartIndexYReal4 => arrayGetStartIndexY, &
                                 array2DGetEndIndexYReal4 => arrayGetEndIndexY

   USE moduleArray2DReal8, ONLY : array2DprintInformationReal8 => printInformation,    &
                                 array2DDestroyReal8    =>  arrayDestroy,      &
                                 array2DSetSizeReal8    =>  arraySetSize,      &
                                 array2DGetSizeXReal8    =>  arrayGetSizeX,      &
                                 array2DGetSizeYReal8    =>  arrayGetSizeY,      &
                                 array2DSetToZeroReal8  =>  arraySetToZero,    &
                                 array2DSetToValueReal8 =>  arraySetToValue,   &
                                 array2DNorm1Real8      =>  arrayNorm1,        &
                                 array2DNorm2Real8      =>  arrayNorm2,        &
                                 array2DNormInfinityReal8  =>  arrayNormInfinity,  &
                                 array2DSqrtReal8       =>  arraySqrt,         &
                                 array2DSumReal8        =>  arraySum,          &
                                 array2DMinReal8        =>  arrayMin,          &
                                 array2DMaxReal8        =>  arrayMax,          &
                                 array2DInsertValueReal8   =>  arrayInsertValue,   &
                                 array2DAddValueReal8   =>  arrayAddValue,     &
                                 array2DScaleReal8      =>  arrayScale,        &
                                 array2DGetValueReal8   =>  arrayGetValue,     &
                                 array2DNormReal8       =>  arrayNorm,         &
                                 array2DCreateBaseReal8 =>  arrayCreateBase,   &
                                 array2DCreateWithDimensionReal8 =>  arrayCreateWithDimension,   &
                                 array2DCreateWithDimensionAndStartingPointReal8 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array2DGetValuesReal8  => arrayGetValues,        &
                                 array2DGetStartIndexXReal8 => arrayGetStartIndexX, &
                                 array2DGetEndIndexXReal8 => arrayGetEndIndexX, &
                                 array2DGetStartIndexYReal8 => arrayGetStartIndexY, &
                                 array2DGetEndIndexYReal8 => arrayGetEndIndexY

   USE mathDynamicMemory, ONLY : initialiseDynamicMemory => initialise, mathSetMemoryIncreaseSize
   
! Procedures status
! =================
   PUBLIC :: initialise, arraySetMemoryIncreaseSize

! Interface
! =========
   INTERFACE matrixCreate
      MODULE PROCEDURE array2DCreateBaseReal8, array2DCreateWithDimensionReal8, array2DCreateWithDimensionAndStartingPointReal8, &
                       array2DCreateBaseReal4, array2DCreateWithDimensionReal4, array2DCreateWithDimensionAndStartingPointReal4, &
               array2DCreateBaseInteger2, array2DCreateWithDimensionInteger2, array2DCreateWithDimensionAndStartingPointInteger2, &
               array2DCreateBaseInteger4, array2DCreateWithDimensionInteger4, array2DCreateWithDimensionAndStartingPointInteger4, &
               array2DCreateBaseInteger8, array2DCreateWithDimensionInteger8, array2DCreateWithDimensionAndStartingPointInteger8
   END INTERFACE

   INTERFACE matrixPrintInformation
      MODULE PROCEDURE array2DprintInformationReal8, array2DprintInformationReal4, &
                       array2DprintInformationInteger2, array2DprintInformationInteger4, array2DprintInformationInteger8
   END INTERFACE

   INTERFACE matrixDestroy
      MODULE PROCEDURE array2DDestroyReal8, array2DDestroyReal4, &
                       array2DDestroyInteger2, array2DDestroyInteger4, array2DDestroyInteger8
   END INTERFACE

   INTERFACE matrixSetSize
      MODULE PROCEDURE array2DSetSizeReal8, array2DSetSizeReal4, &
                       array2DSetSizeInteger2, array2DSetSizeInteger4, array2DSetSizeInteger8
   END INTERFACE

   INTERFACE matrixGetSizeX
      MODULE PROCEDURE array2DGetSizeXReal8, array2DGetSizeXReal4,&
                       array2DGetSizeXInteger2, array2DGetSizeXInteger4, array2DGetSizeXInteger8
   END INTERFACE
   INTERFACE matrixGetSizeY
      MODULE PROCEDURE array2DGetSizeYReal8, array2DGetSizeYReal4,&
                       array2DGetSizeYInteger2, array2DGetSizeYInteger4, array2DGetSizeYInteger8
   END INTERFACE

   INTERFACE matrixSetToZero
      MODULE PROCEDURE array2DSetToZeroReal8, array2DSetToZeroReal4, &
                       array2DSetToZeroInteger2, array2DSetToZeroInteger4, array2DSetToZeroInteger8
   END INTERFACE

   INTERFACE matrixSetToValue
      MODULE PROCEDURE array2DSetToValueReal8, array2DSetToValueReal4, &
                       array2DSetToValueInteger2, array2DSetToValueInteger4, array2DSetToValueInteger8
   END INTERFACE

   INTERFACE matrixNorm1
      MODULE PROCEDURE array2DNorm1Real8, array2DNorm1Real4
   END INTERFACE

   INTERFACE matrixNorm2
      MODULE PROCEDURE array2DNorm2Real8, array2DNorm2Real4
   END INTERFACE

   INTERFACE matrixNormInfinity
      MODULE PROCEDURE  array2DNormInfinityReal8, array2DNormInfinityReal4
   END INTERFACE

   INTERFACE matrixSqrt
      MODULE PROCEDURE array2DSqrtReal8, array2DSqrtReal4
   END INTERFACE

   INTERFACE matrixSum
      MODULE PROCEDURE array2DSumReal8, array2DSumReal4
   END INTERFACE

   INTERFACE matrixMin
      MODULE PROCEDURE array2DMinReal8, array2DMinReal4, &
                       array2DMinInteger2, array2DMinInteger4, array2DMinInteger8
   END INTERFACE

   INTERFACE matrixMax
      MODULE PROCEDURE array2DMaxReal8, array2DMaxReal4, &
                       array2DMaxInteger2, array2DMaxInteger4, array2DMaxInteger8
   END INTERFACE

   INTERFACE matrixInsertValue
      MODULE PROCEDURE array2DInsertValueReal8, array2DInsertValueReal4, &
                       array2DInsertValueInteger2, array2DInsertValueInteger4, array2DInsertValueInteger8
   END INTERFACE

   INTERFACE matrixAddValue
      MODULE PROCEDURE array2DAddValueReal8, array2DAddValueReal4, &
                       array2DAddValueInteger2, array2DAddValueInteger4, array2DAddValueInteger8
   END INTERFACE

   INTERFACE matrixScale
      MODULE PROCEDURE array2DScaleReal8, array2DScaleReal4
   END INTERFACE

   INTERFACE matrixGetValue
      MODULE PROCEDURE array2DGetValueReal8, array2DGetValueReal4, &
                       array2DGetValueInteger2, array2DGetValueInteger4, array2DGetValueInteger8
   END INTERFACE

   INTERFACE matrixNorm
      MODULE PROCEDURE array2DNormReal8, array2DNormReal4
   END INTERFACE

   INTERFACE matrixGetValues
      MODULE PROCEDURE array2DGetValuesReal8, array2DGetValuesReal4, &
                       array2DGetValuesInteger2, array2DGetValuesInteger4, array2DGetValuesInteger8
   END INTERFACE

   INTERFACE matrixGetStartIndexX
      MODULE PROCEDURE array2DGetStartIndexXReal8, array2DGetStartIndexXReal4, &
                       array2DGetStartIndexXInteger2, array2DGetStartIndexXInteger4, array2DGetStartIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetEndIndexX
      MODULE PROCEDURE array2DGetEndIndexXReal8, array2DGetEndIndexXReal4, &
                       array2DGetEndIndexXInteger2, array2DGetEndIndexXInteger4, array2DGetEndIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetStartIndexY
      MODULE PROCEDURE array2DGetStartIndexYReal8, array2DGetStartIndexYReal4, &
                       array2DGetStartIndexYInteger2, array2DGetStartIndexYInteger4, array2DGetStartIndexYInteger8
   END INTERFACE

   INTERFACE matrixGetEndIndexY
      MODULE PROCEDURE array2DGetEndIndexYReal8, array2DGetEndIndexYReal4, &
                       array2DGetEndIndexYInteger2, array2DGetEndIndexYInteger4, array2DGetEndIndexYInteger8
   END INTERFACE

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS

! ============================================================
! ===            External procedure ("PUBLIC")             ===
! ============================================================

! Procedure 1 : initialisation
! ----------------------------
  SUBROUTINE initialise()
  
!     Body
!     - - -
      CALL initialiseDynamicMemory()

  END SUBROUTINE

! Procedure 2 : define the extra size for allocate array
! --------------------------------------------------------
  SUBROUTINE arraySetMemoryIncreaseSize(extraSizeX,extraSizeY,extraSizeZ)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: extraSizeX,extraSizeY
      INTEGER, OPTIONAL, INTENT(IN) :: extraSizeZ

!     Body
!     - - -
      IF ( PRESENT(extraSizeZ) ) THEN
          CALL mathSetMemoryIncreaseSize(extraSizeX,extraSizeY,extraSizeZ)
      ELSE
          CALL mathSetMemoryIncreaseSize(extraSizeX,extraSizeY)
      END IF

  END SUBROUTINE

END MODULE matrixInterface

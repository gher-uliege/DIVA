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

   USE array3DInteger2Definition
   USE array3DInteger4Definition
   USE array3DInteger8Definition
   USE array3DReal4Definition
   USE array3DReal8Definition

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
                                 array2DGetEndIndexYInteger2 => arrayGetEndIndexY, &
                                 array2DSetIncreaseSizeInteger2 => arraySetIncreaseSize

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
                                 array2DGetEndIndexYInteger4 => arrayGetEndIndexY, &
                                 array2DSetIncreaseSizeInteger4 => arraySetIncreaseSize

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
                                 array2DGetEndIndexYInteger8 => arrayGetEndIndexY, &
                                 array2DSetIncreaseSizeInteger8 => arraySetIncreaseSize

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
                                 array2DGetEndIndexYReal4 => arrayGetEndIndexY, &
                                 array2DSetIncreaseSizeReal4 => arraySetIncreaseSize

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
                                 array2DGetEndIndexYReal8 => arrayGetEndIndexY, &
                                 array2DSetIncreaseSizeReal8 => arraySetIncreaseSize

   USE moduleArray3DInteger2, ONLY : array3DprintInformationInteger2 => printInformation,    &
                                 array3DDestroyInteger2    =>  arrayDestroy,      &
                                 array3DSetSizeInteger2    =>  arraySetSize,      &
                                 array3DGetSizeXInteger2    =>  arrayGetSizeX,      &
                                 array3DGetSizeYInteger2    =>  arrayGetSizeY,      &
                                 array3DGetSizeZInteger2    =>  arrayGetSizeZ,      &
                                 array3DSetToZeroInteger2  =>  arraySetToZero,    &
                                 array3DSetToValueInteger2 =>  arraySetToValue,   &
                                 array3DMinInteger2        =>  arrayMin,          &
                                 array3DMaxInteger2        =>  arrayMax,          &
                                 array3DInsertValueInteger2   =>  arrayInsertValue,   &
                                 array3DAddValueInteger2   =>  arrayAddValue,     &
                                 array3DGetValueInteger2   =>  arrayGetValue,     &
                                 array3DCreateBaseInteger2 =>  arrayCreateBase,   &
                                 array3DCreateWithDimensionInteger2 =>  arrayCreateWithDimension,   &
                                 array3DCreateWithDimensionAndStartingPointInteger2 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array3DGetValuesInteger2  => arrayGetValues,        &
                                 array3DGetStartIndexXInteger2 => arrayGetStartIndexX, &
                                 array3DGetEndIndexXInteger2 => arrayGetEndIndexX ,&
                                 array3DGetStartIndexYInteger2 => arrayGetStartIndexY, &
                                 array3DGetEndIndexYInteger2 => arrayGetEndIndexY, &
                                 array3DGetStartIndexZInteger2 => arrayGetStartIndexZ, &
                                 array3DGetEndIndexZInteger2 => arrayGetEndIndexZ, &
                                 array3DSetIncreaseSizeInteger2 => arraySetIncreaseSize

   USE moduleArray3DInteger4, ONLY : array3DprintInformationInteger4 => printInformation,    &
                                 array3DDestroyInteger4    =>  arrayDestroy,      &
                                 array3DSetSizeInteger4    =>  arraySetSize,      &
                                 array3DGetSizeXInteger4    =>  arrayGetSizeX,      &
                                 array3DGetSizeYInteger4    =>  arrayGetSizeY,      &
                                 array3DGetSizeZInteger4    =>  arrayGetSizeZ,      &
                                 array3DSetToZeroInteger4  =>  arraySetToZero,    &
                                 array3DSetToValueInteger4 =>  arraySetToValue,   &
                                 array3DMinInteger4        =>  arrayMin,          &
                                 array3DMaxInteger4        =>  arrayMax,          &
                                 array3DInsertValueInteger4   =>  arrayInsertValue,   &
                                 array3DAddValueInteger4   =>  arrayAddValue,     &
                                 array3DGetValueInteger4   =>  arrayGetValue,     &
                                 array3DCreateBaseInteger4 =>  arrayCreateBase,   &
                                 array3DCreateWithDimensionInteger4 =>  arrayCreateWithDimension,   &
                                 array3DCreateWithDimensionAndStartingPointInteger4 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array3DGetValuesInteger4  => arrayGetValues,        &
                                 array3DGetStartIndexXInteger4 => arrayGetStartIndexX, &
                                 array3DGetEndIndexXInteger4 => arrayGetEndIndexX, &
                                 array3DGetStartIndexYInteger4 => arrayGetStartIndexY, &
                                 array3DGetEndIndexYInteger4 => arrayGetEndIndexY, &
                                 array3DGetStartIndexZInteger4 => arrayGetStartIndexZ, &
                                 array3DGetEndIndexZInteger4 => arrayGetEndIndexZ, &
                                 array3DSetIncreaseSizeInteger4 => arraySetIncreaseSize

   USE moduleArray3DInteger8, ONLY : array3DprintInformationInteger8 => printInformation,    &
                                 array3DDestroyInteger8    =>  arrayDestroy,      &
                                 array3DSetSizeInteger8    =>  arraySetSize,      &
                                 array3DGetSizeXInteger8    =>  arrayGetSizeX,      &
                                 array3DGetSizeYInteger8    =>  arrayGetSizeY,      &
                                 array3DGetSizeZInteger8    =>  arrayGetSizeZ,      &
                                 array3DSetToZeroInteger8  =>  arraySetToZero,    &
                                 array3DSetToValueInteger8 =>  arraySetToValue,   &
                                 array3DMinInteger8        =>  arrayMin,          &
                                 array3DMaxInteger8        =>  arrayMax,          &
                                 array3DInsertValueInteger8   =>  arrayInsertValue,   &
                                 array3DAddValueInteger8   =>  arrayAddValue,     &
                                 array3DGetValueInteger8   =>  arrayGetValue,     &
                                 array3DCreateBaseInteger8 =>  arrayCreateBase,   &
                                 array3DCreateWithDimensionInteger8 =>  arrayCreateWithDimension,   &
                                 array3DCreateWithDimensionAndStartingPointInteger8 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array3DGetValuesInteger8  => arrayGetValues,        &
                                 array3DGetStartIndexXInteger8 => arrayGetStartIndexX, &
                                 array3DGetEndIndexXInteger8 => arrayGetEndIndexX, &
                                 array3DGetStartIndexYInteger8 => arrayGetStartIndexY, &
                                 array3DGetEndIndexYInteger8 => arrayGetEndIndexY, &
                                 array3DGetStartIndexZInteger8 => arrayGetStartIndexZ, &
                                 array3DGetEndIndexZInteger8 => arrayGetEndIndexZ, &
                                 array3DSetIncreaseSizeInteger8 => arraySetIncreaseSize

   USE moduleArray3DReal4, ONLY : array3DprintInformationReal4 => printInformation,    &
                                 array3DDestroyReal4    =>  arrayDestroy,      &
                                 array3DSetSizeReal4    =>  arraySetSize,      &
                                 array3DGetSizeXReal4    =>  arrayGetSizeX,      &
                                 array3DGetSizeYReal4    =>  arrayGetSizeY,      &
                                 array3DGetSizeZReal4    =>  arrayGetSizeZ,      &
                                 array3DSetToZeroReal4  =>  arraySetToZero,    &
                                 array3DSetToValueReal4 =>  arraySetToValue,   &
                                 array3DNorm1Real4      =>  arrayNorm1,        &
                                 array3DNorm2Real4      =>  arrayNorm2,        &
                                 array3DNormInfinityReal4  =>  arrayNormInfinity,  &
                                 array3DSqrtReal4       =>  arraySqrt,         &
                                 array3DSumReal4        =>  arraySum,          &
                                 array3DMinReal4        =>  arrayMin,          &
                                 array3DMaxReal4        =>  arrayMax,          &
                                 array3DInsertValueReal4   =>  arrayInsertValue,   &
                                 array3DAddValueReal4   =>  arrayAddValue,     &
                                 array3DScaleReal4      =>  arrayScale,        &
                                 array3DGetValueReal4   =>  arrayGetValue,     &
                                 array3DNormReal4       =>  arrayNorm,         &
                                 array3DCreateBaseReal4 =>  arrayCreateBase,   &
                                 array3DCreateWithDimensionReal4 =>  arrayCreateWithDimension,   &
                                 array3DCreateWithDimensionAndStartingPointReal4 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array3DGetValuesReal4  => arrayGetValues,        &
                                 array3DGetStartIndexXReal4 => arrayGetStartIndexX, &
                                 array3DGetEndIndexXReal4 => arrayGetEndIndexX,    &
                                 array3DGetStartIndexYReal4 => arrayGetStartIndexY, &
                                 array3DGetEndIndexYReal4 => arrayGetEndIndexY, &
                                 array3DGetStartIndexZReal4 => arrayGetStartIndexZ, &
                                 array3DGetEndIndexZReal4 => arrayGetEndIndexZ, &
                                 array3DSetIncreaseSizeReal4 => arraySetIncreaseSize

   USE moduleArray3DReal8, ONLY : array3DprintInformationReal8 => printInformation,    &
                                 array3DDestroyReal8    =>  arrayDestroy,      &
                                 array3DSetSizeReal8    =>  arraySetSize,      &
                                 array3DGetSizeXReal8    =>  arrayGetSizeX,      &
                                 array3DGetSizeYReal8    =>  arrayGetSizeY,      &
                                 array3DGetSizeZReal8    =>  arrayGetSizeZ,      &
                                 array3DSetToZeroReal8  =>  arraySetToZero,    &
                                 array3DSetToValueReal8 =>  arraySetToValue,   &
                                 array3DNorm1Real8      =>  arrayNorm1,        &
                                 array3DNorm2Real8      =>  arrayNorm2,        &
                                 array3DNormInfinityReal8  =>  arrayNormInfinity,  &
                                 array3DSqrtReal8       =>  arraySqrt,         &
                                 array3DSumReal8        =>  arraySum,          &
                                 array3DMinReal8        =>  arrayMin,          &
                                 array3DMaxReal8        =>  arrayMax,          &
                                 array3DInsertValueReal8   =>  arrayInsertValue,   &
                                 array3DAddValueReal8   =>  arrayAddValue,     &
                                 array3DScaleReal8      =>  arrayScale,        &
                                 array3DGetValueReal8   =>  arrayGetValue,     &
                                 array3DNormReal8       =>  arrayNorm,         &
                                 array3DCreateBaseReal8 =>  arrayCreateBase,   &
                                 array3DCreateWithDimensionReal8 =>  arrayCreateWithDimension,   &
                                 array3DCreateWithDimensionAndStartingPointReal8 =>  arrayCreateWithDimensionAndStartingPoint, &
                                 array3DGetValuesReal8  => arrayGetValues,        &
                                 array3DGetStartIndexXReal8 => arrayGetStartIndexX, &
                                 array3DGetEndIndexXReal8 => arrayGetEndIndexX, &
                                 array3DGetStartIndexYReal8 => arrayGetStartIndexY, &
                                 array3DGetEndIndexYReal8 => arrayGetEndIndexY, &
                                 array3DGetStartIndexZReal8 => arrayGetStartIndexZ, &
                                 array3DGetEndIndexZReal8 => arrayGetEndIndexZ, &
                                 array3DSetIncreaseSizeReal8 => arraySetIncreaseSize

   USE mathDynamicMemory, ONLY : initialiseDynamicMemory => initialise, mathSetMemoryIncreaseSize
   
! Procedures status
! =================
   PUBLIC :: initialise, arraySetDefaultMemoryIncreaseSize

! Interface
! =========
   INTERFACE matrixCreate
      MODULE PROCEDURE array2DCreateBaseReal8, array2DCreateWithDimensionReal8, array2DCreateWithDimensionAndStartingPointReal8, &
                       array2DCreateBaseReal4, array2DCreateWithDimensionReal4, array2DCreateWithDimensionAndStartingPointReal4, &
               array2DCreateBaseInteger2, array2DCreateWithDimensionInteger2, array2DCreateWithDimensionAndStartingPointInteger2, &
               array2DCreateBaseInteger4, array2DCreateWithDimensionInteger4, array2DCreateWithDimensionAndStartingPointInteger4, &
               array2DCreateBaseInteger8, array2DCreateWithDimensionInteger8, array2DCreateWithDimensionAndStartingPointInteger8, &
               array3DCreateBaseReal8, array3DCreateWithDimensionReal8, array3DCreateWithDimensionAndStartingPointReal8, &
               array3DCreateBaseReal4, array3DCreateWithDimensionReal4, array3DCreateWithDimensionAndStartingPointReal4, &
               array3DCreateBaseInteger2, array3DCreateWithDimensionInteger2, array3DCreateWithDimensionAndStartingPointInteger2, &
               array3DCreateBaseInteger4, array3DCreateWithDimensionInteger4, array3DCreateWithDimensionAndStartingPointInteger4, &
               array3DCreateBaseInteger8, array3DCreateWithDimensionInteger8, array3DCreateWithDimensionAndStartingPointInteger8
   END INTERFACE

   INTERFACE matrixPrintInformation
      MODULE PROCEDURE array2DprintInformationReal8, array2DprintInformationReal4, &
                       array2DprintInformationInteger2, array2DprintInformationInteger4, array2DprintInformationInteger8, &
                       array3DprintInformationReal8, array3DprintInformationReal4, &
                       array3DprintInformationInteger2, array3DprintInformationInteger4, array3DprintInformationInteger8
   END INTERFACE

   INTERFACE matrixDestroy
      MODULE PROCEDURE array2DDestroyReal8, array2DDestroyReal4, &
                       array2DDestroyInteger2, array2DDestroyInteger4, array2DDestroyInteger8, &
                       array3DDestroyReal8, array3DDestroyReal4, &
                       array3DDestroyInteger2, array3DDestroyInteger4, array3DDestroyInteger8
   END INTERFACE

   INTERFACE matrixSetSize
      MODULE PROCEDURE array2DSetSizeReal8, array2DSetSizeReal4, &
                       array2DSetSizeInteger2, array2DSetSizeInteger4, array2DSetSizeInteger8, &
                       array3DSetSizeReal8, array3DSetSizeReal4, &
                       array3DSetSizeInteger2, array3DSetSizeInteger4, array3DSetSizeInteger8
   END INTERFACE

   INTERFACE matrixGetSizeX
      MODULE PROCEDURE array2DGetSizeXReal8, array2DGetSizeXReal4,&
                       array2DGetSizeXInteger2, array2DGetSizeXInteger4, array2DGetSizeXInteger8, &
                       array3DGetSizeXReal8, array3DGetSizeXReal4,&
                       array3DGetSizeXInteger2, array3DGetSizeXInteger4, array3DGetSizeXInteger8
   END INTERFACE
   INTERFACE matrixGetSizeY
      MODULE PROCEDURE array2DGetSizeYReal8, array2DGetSizeYReal4,&
                       array2DGetSizeYInteger2, array2DGetSizeYInteger4, array2DGetSizeYInteger8, &
                       array3DGetSizeYReal8, array3DGetSizeYReal4,&
                       array3DGetSizeYInteger2, array3DGetSizeYInteger4, array3DGetSizeYInteger8
   END INTERFACE
   INTERFACE matrixGetSizeZ
      MODULE PROCEDURE array3DGetSizeZReal8, array3DGetSizeZReal4,&
                       array3DGetSizeZInteger2, array3DGetSizeZInteger4, array3DGetSizeZInteger8
   END INTERFACE

   INTERFACE matrixSetToZero
      MODULE PROCEDURE array2DSetToZeroReal8, array2DSetToZeroReal4, &
                       array2DSetToZeroInteger2, array2DSetToZeroInteger4, array2DSetToZeroInteger8, &
                       array3DSetToZeroReal8, array3DSetToZeroReal4, &
                       array3DSetToZeroInteger2, array3DSetToZeroInteger4, array3DSetToZeroInteger8
   END INTERFACE

   INTERFACE matrixSetToValue
      MODULE PROCEDURE array2DSetToValueReal8, array2DSetToValueReal4, &
                       array2DSetToValueInteger2, array2DSetToValueInteger4, array2DSetToValueInteger8, &
                       array3DSetToValueReal8, array3DSetToValueReal4, &
                       array3DSetToValueInteger2, array3DSetToValueInteger4, array3DSetToValueInteger8
   END INTERFACE

   INTERFACE matrixNorm1
      MODULE PROCEDURE array2DNorm1Real8, array2DNorm1Real4, array3DNorm1Real8, array3DNorm1Real4
   END INTERFACE

   INTERFACE matrixNorm2
      MODULE PROCEDURE array2DNorm2Real8, array2DNorm2Real4, array3DNorm2Real8, array3DNorm2Real4
   END INTERFACE

   INTERFACE matrixNormInfinity
      MODULE PROCEDURE  array2DNormInfinityReal8, array2DNormInfinityReal4, array3DNormInfinityReal8, array3DNormInfinityReal4
   END INTERFACE

   INTERFACE matrixSqrt
      MODULE PROCEDURE array2DSqrtReal8, array2DSqrtReal4, array3DSqrtReal8, array3DSqrtReal4
   END INTERFACE

   INTERFACE matrixSum
      MODULE PROCEDURE array2DSumReal8, array2DSumReal4, array3DSumReal8, array3DSumReal4
   END INTERFACE

   INTERFACE matrixMin
      MODULE PROCEDURE array2DMinReal8, array2DMinReal4, &
                       array2DMinInteger2, array2DMinInteger4, array2DMinInteger8, &
                       array3DMinReal8, array3DMinReal4, &
                       array3DMinInteger2, array3DMinInteger4, array3DMinInteger8
   END INTERFACE

   INTERFACE matrixMax
      MODULE PROCEDURE array2DMaxReal8, array2DMaxReal4, &
                       array2DMaxInteger2, array2DMaxInteger4, array2DMaxInteger8, &
                       array3DMaxReal8, array3DMaxReal4, &
                       array3DMaxInteger2, array3DMaxInteger4, array3DMaxInteger8
   END INTERFACE

   INTERFACE matrixInsertValue
      MODULE PROCEDURE array2DInsertValueReal8, array2DInsertValueReal4, &
                       array2DInsertValueInteger2, array2DInsertValueInteger4, array2DInsertValueInteger8, &
                       array3DInsertValueReal8, array3DInsertValueReal4, &
                       array3DInsertValueInteger2, array3DInsertValueInteger4, array3DInsertValueInteger8
   END INTERFACE

   INTERFACE matrixAddValue
      MODULE PROCEDURE array2DAddValueReal8, array2DAddValueReal4, &
                       array2DAddValueInteger2, array2DAddValueInteger4, array2DAddValueInteger8, &
                       array3DAddValueReal8, array3DAddValueReal4, &
                       array3DAddValueInteger2, array3DAddValueInteger4, array3DAddValueInteger8
   END INTERFACE

   INTERFACE matrixScale
      MODULE PROCEDURE array2DScaleReal8, array2DScaleReal4, array3DScaleReal8, array3DScaleReal4
   END INTERFACE

   INTERFACE matrixGetValue
      MODULE PROCEDURE array2DGetValueReal8, array2DGetValueReal4, &
                       array2DGetValueInteger2, array2DGetValueInteger4, array2DGetValueInteger8, &
                       array3DGetValueReal8, array3DGetValueReal4, &
                       array3DGetValueInteger2, array3DGetValueInteger4, array3DGetValueInteger8
   END INTERFACE

   INTERFACE matrixNorm
      MODULE PROCEDURE array2DNormReal8, array2DNormReal4, array3DNormReal8, array3DNormReal4
   END INTERFACE

   INTERFACE matrixGetValues
      MODULE PROCEDURE array2DGetValuesReal8, array2DGetValuesReal4, &
                       array2DGetValuesInteger2, array2DGetValuesInteger4, array2DGetValuesInteger8, &
                       array3DGetValuesReal8, array3DGetValuesReal4, &
                       array3DGetValuesInteger2, array3DGetValuesInteger4, array3DGetValuesInteger8
   END INTERFACE

   INTERFACE matrixGetStartIndexX
      MODULE PROCEDURE array2DGetStartIndexXReal8, array2DGetStartIndexXReal4, &
                       array2DGetStartIndexXInteger2, array2DGetStartIndexXInteger4, array2DGetStartIndexXInteger8, &
                       array3DGetStartIndexXReal8, array3DGetStartIndexXReal4, &
                       array3DGetStartIndexXInteger2, array3DGetStartIndexXInteger4, array3DGetStartIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetEndIndexX
      MODULE PROCEDURE array2DGetEndIndexXReal8, array2DGetEndIndexXReal4, &
                       array2DGetEndIndexXInteger2, array2DGetEndIndexXInteger4, array2DGetEndIndexXInteger8, &
                       array3DGetEndIndexXReal8, array3DGetEndIndexXReal4, &
                       array3DGetEndIndexXInteger2, array3DGetEndIndexXInteger4, array3DGetEndIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetStartIndexY
      MODULE PROCEDURE array2DGetStartIndexYReal8, array2DGetStartIndexYReal4, &
                       array2DGetStartIndexYInteger2, array2DGetStartIndexYInteger4, array2DGetStartIndexYInteger8, &
                       array3DGetStartIndexYReal8, array3DGetStartIndexYReal4, &
                       array3DGetStartIndexYInteger2, array3DGetStartIndexYInteger4, array3DGetStartIndexYInteger8
   END INTERFACE

   INTERFACE matrixGetEndIndexY
      MODULE PROCEDURE array2DGetEndIndexYReal8, array2DGetEndIndexYReal4, &
                       array2DGetEndIndexYInteger2, array2DGetEndIndexYInteger4, array2DGetEndIndexYInteger8, &
                       array3DGetEndIndexYReal8, array3DGetEndIndexYReal4, &
                       array3DGetEndIndexYInteger2, array3DGetEndIndexYInteger4, array3DGetEndIndexYInteger8
   END INTERFACE

   INTERFACE matrixGetStartIndexZ
      MODULE PROCEDURE array3DGetStartIndexZReal8, array3DGetStartIndexZReal4, &
                       array3DGetStartIndexZInteger2, array3DGetStartIndexZInteger4, array3DGetStartIndexZInteger8
   END INTERFACE

   INTERFACE matrixGetEndIndexZ
      MODULE PROCEDURE array3DGetEndIndexZReal8, array3DGetEndIndexZReal4, &
                       array3DGetEndIndexZInteger2, array3DGetEndIndexZInteger4, array3DGetEndIndexZInteger8
   END INTERFACE

   INTERFACE matrixSetIncreaseSize
      MODULE PROCEDURE array2DSetIncreaseSizeReal8, array2DSetIncreaseSizeReal4, &
                       array2DSetIncreaseSizeInteger2, array2DSetIncreaseSizeInteger4, array2DSetIncreaseSizeInteger8, &
                       array3DSetIncreaseSizeReal8, array3DSetIncreaseSizeReal4, &
                       array3DSetIncreaseSizeInteger2, array3DSetIncreaseSizeInteger4, array3DSetIncreaseSizeInteger8
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
  SUBROUTINE arraySetDefaultMemoryIncreaseSize(extraSizeX,extraSizeY,extraSizeZ)

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

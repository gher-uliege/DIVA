MODULE vectorInterface
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
   USE vectorInteger2Definition
   USE vectorInteger4Definition
   USE vectorInteger8Definition
   USE vectorReal4Definition
   USE vectorReal8Definition

   USE moduleVectorInteger2, ONLY : printInformationInteger2 => printInformation,    &
                                 vectorDestroyInteger2    =>  vectorDestroy,      &
                                 vectorSetSizeInteger2    =>  vectorSetSize,      &
                                 vectorGetSizeInteger2    =>  vectorGetSize,      &
                                 vectorSetToZeroInteger2  =>  vectorSetToZero,    &
                                 vectorSetToValueInteger2 =>  vectorSetToValue,   &
                                 vectorMinInteger2        =>  vectorMin,          &
                                 vectorMaxInteger2        =>  vectorMax,          &
                                 vectorInsertValueInteger2   =>  vectorInsertValue,   &
                                 vectorAddValueInteger2   =>  vectorAddValue,     &
                                 vectorGetValueInteger2   =>  vectorGetValue,     &
                                 vectorCreateBaseInteger2 =>  vectorCreateBase,   &
                                 vectorCreateWithDimensionInteger2 =>  vectorCreateWithDimension,   &
                                 vectorCreateWithDimensionAndStartingPointInteger2 =>  vectorCreateWithDimensionAndStartingPoint, &
                                 vectorGetValuesInteger2  => vectorGetValues,        &
                                 vectorGetStartIndexInteger2 => vectorGetStartIndex, &
                                 vectorGetEndIndexInteger2 => vectorGetEndIndex

   USE moduleVectorInteger4, ONLY : printInformationInteger4 => printInformation,    &
                                 vectorDestroyInteger4    =>  vectorDestroy,      &
                                 vectorSetSizeInteger4    =>  vectorSetSize,      &
                                 vectorGetSizeInteger4    =>  vectorGetSize,      &
                                 vectorSetToZeroInteger4  =>  vectorSetToZero,    &
                                 vectorSetToValueInteger4 =>  vectorSetToValue,   &
                                 vectorMinInteger4        =>  vectorMin,          &
                                 vectorMaxInteger4        =>  vectorMax,          &
                                 vectorInsertValueInteger4   =>  vectorInsertValue,   &
                                 vectorAddValueInteger4   =>  vectorAddValue,     &
                                 vectorGetValueInteger4   =>  vectorGetValue,     &
                                 vectorCreateBaseInteger4 =>  vectorCreateBase,   &
                                 vectorCreateWithDimensionInteger4 =>  vectorCreateWithDimension,   &
                                 vectorCreateWithDimensionAndStartingPointInteger4 =>  vectorCreateWithDimensionAndStartingPoint, &
                                 vectorGetValuesInteger4  => vectorGetValues,        &
                                 vectorGetStartIndexInteger4 => vectorGetStartIndex, &
                                 vectorGetEndIndexInteger4 => vectorGetEndIndex

   USE moduleVectorInteger8, ONLY : printInformationInteger8 => printInformation,    &
                                 vectorDestroyInteger8    =>  vectorDestroy,      &
                                 vectorSetSizeInteger8    =>  vectorSetSize,      &
                                 vectorGetSizeInteger8    =>  vectorGetSize,      &
                                 vectorSetToZeroInteger8  =>  vectorSetToZero,    &
                                 vectorSetToValueInteger8 =>  vectorSetToValue,   &
                                 vectorMinInteger8        =>  vectorMin,          &
                                 vectorMaxInteger8        =>  vectorMax,          &
                                 vectorInsertValueInteger8   =>  vectorInsertValue,   &
                                 vectorAddValueInteger8   =>  vectorAddValue,     &
                                 vectorGetValueInteger8   =>  vectorGetValue,     &
                                 vectorCreateBaseInteger8 =>  vectorCreateBase,   &
                                 vectorCreateWithDimensionInteger8 =>  vectorCreateWithDimension,   &
                                 vectorCreateWithDimensionAndStartingPointInteger8 =>  vectorCreateWithDimensionAndStartingPoint, &
                                 vectorGetValuesInteger8  => vectorGetValues,        &
                                 vectorGetStartIndexInteger8 => vectorGetStartIndex, &
                                 vectorGetEndIndexInteger8 => vectorGetEndIndex

   USE moduleVectorReal4, ONLY : printInformationReal4 => printInformation,    &
                                 vectorDestroyReal4    =>  vectorDestroy,      &
                                 vectorSetSizeReal4    =>  vectorSetSize,      &
                                 vectorGetSizeReal4    =>  vectorGetSize,      &
                                 vectorSetToZeroReal4  =>  vectorSetToZero,    &
                                 vectorSetToValueReal4 =>  vectorSetToValue,   &
                                 vectorNorm1Real4      =>  vectorNorm1,        &
                                 vectorNorm2Real4      =>  vectorNorm2,        &
                                 vectorNormInfinityReal4  =>  vectorNormInfinity,  &
                                 vectorSqrtReal4       =>  vectorSqrt,         &
                                 vectorSumReal4        =>  vectorSum,          &
                                 vectorMinReal4        =>  vectorMin,          &
                                 vectorMaxReal4        =>  vectorMax,          &
                                 vectorInsertValueReal4   =>  vectorInsertValue,   &
                                 vectorAddValueReal4   =>  vectorAddValue,     &
                                 vectorScaleReal4      =>  vectorScale,        &
                                 vectorDotReal4        =>  vectorDot,          &
                                 vectorGetValueReal4   =>  vectorGetValue,     &
                                 vectorNormReal4       =>  vectorNorm,         &
                                 vectorCreateBaseReal4 =>  vectorCreateBase,   &
                                 vectorCreateWithDimensionReal4 =>  vectorCreateWithDimension,   &
                                 vectorCreateWithDimensionAndStartingPointReal4 =>  vectorCreateWithDimensionAndStartingPoint, &
                                 vectorGetValuesReal4  => vectorGetValues,        &
                                 vectorGetStartIndexReal4 => vectorGetStartIndex, &
                                 vectorGetEndIndexReal4 => vectorGetEndIndex

   USE moduleVectorReal8, ONLY : printInformationReal8 => printInformation,    &
                                 vectorDestroyReal8    =>  vectorDestroy,      &
                                 vectorSetSizeReal8    =>  vectorSetSize,      &
                                 vectorGetSizeReal8    =>  vectorGetSize,      &
                                 vectorSetToZeroReal8  =>  vectorSetToZero,    &
                                 vectorSetToValueReal8 =>  vectorSetToValue,   &
                                 vectorNorm1Real8      =>  vectorNorm1,        &
                                 vectorNorm2Real8      =>  vectorNorm2,        &
                                 vectorNormInfinityReal8  =>  vectorNormInfinity,  &
                                 vectorSqrtReal8       =>  vectorSqrt,         &
                                 vectorSumReal8        =>  vectorSum,          &
                                 vectorMinReal8        =>  vectorMin,          &
                                 vectorMaxReal8        =>  vectorMax,          &
                                 vectorInsertValueReal8   =>  vectorInsertValue,   &
                                 vectorAddValueReal8   =>  vectorAddValue,     &
                                 vectorScaleReal8      =>  vectorScale,        &
                                 vectorDotReal8        =>  vectorDot,          &
                                 vectorGetValueReal8   =>  vectorGetValue,     &
                                 vectorNormReal8       =>  vectorNorm,         &
                                 vectorCreateBaseReal8 =>  vectorCreateBase,   &
                                 vectorCreateWithDimensionReal8 =>  vectorCreateWithDimension,   &
                                 vectorCreateWithDimensionAndStartingPointReal8 =>  vectorCreateWithDimensionAndStartingPoint, &
                                 vectorGetValuesReal8  => vectorGetValues,        &
                                 vectorGetStartIndexReal8 => vectorGetStartIndex, &
                                 vectorGetEndIndexReal8 => vectorGetEndIndex

   USE mathDynamicMemory, ONLY : initialiseDynamicMemory => initialise, mathSetMemoryIncreaseSize
   
! Procedures status
! =================
   PUBLIC :: initialise, vectorSetMemoryIncreaseSize

! Interface
! =========
   INTERFACE vectorCreate
      MODULE PROCEDURE vectorCreateBaseReal8, vectorCreateWithDimensionReal8, vectorCreateWithDimensionAndStartingPointReal8, &
                       vectorCreateBaseReal4, vectorCreateWithDimensionReal4, vectorCreateWithDimensionAndStartingPointReal4, &
                   vectorCreateBaseInteger2, vectorCreateWithDimensionInteger2, vectorCreateWithDimensionAndStartingPointInteger2, &
                   vectorCreateBaseInteger4, vectorCreateWithDimensionInteger4, vectorCreateWithDimensionAndStartingPointInteger4, &
                   vectorCreateBaseInteger8, vectorCreateWithDimensionInteger8, vectorCreateWithDimensionAndStartingPointInteger8
   END INTERFACE

   INTERFACE printInformation
      MODULE PROCEDURE printInformationReal8, printInformationReal4, &
                       printInformationInteger2, printInformationInteger4, printInformationInteger8
   END INTERFACE

   INTERFACE vectorDestroy
      MODULE PROCEDURE vectorDestroyReal8, vectorDestroyReal4, &
                       vectorDestroyInteger2, vectorDestroyInteger4, vectorDestroyInteger8
   END INTERFACE

   INTERFACE vectorSetSize
      MODULE PROCEDURE vectorSetSizeReal8, vectorSetSizeReal4, &
                       vectorSetSizeInteger2, vectorSetSizeInteger4, vectorSetSizeInteger8
   END INTERFACE

   INTERFACE vectorGetSize
      MODULE PROCEDURE vectorGetSizeReal8, vectorGetSizeReal4,&
                       vectorGetSizeInteger2, vectorGetSizeInteger4, vectorGetSizeInteger8
   END INTERFACE

   INTERFACE vectorSetToZero
      MODULE PROCEDURE vectorSetToZeroReal8, vectorSetToZeroReal4, &
                       vectorSetToZeroInteger2, vectorSetToZeroInteger4, vectorSetToZeroInteger8
   END INTERFACE

   INTERFACE vectorSetToValue
      MODULE PROCEDURE vectorSetToValueReal8, vectorSetToValueReal4, &
                       vectorSetToValueInteger2, vectorSetToValueInteger4, vectorSetToValueInteger8
   END INTERFACE

   INTERFACE vectorNorm1
      MODULE PROCEDURE vectorNorm1Real8, vectorNorm1Real4
   END INTERFACE

   INTERFACE vectorNorm2
      MODULE PROCEDURE vectorNorm2Real8, vectorNorm2Real4
   END INTERFACE

   INTERFACE vectorNormInfinity
      MODULE PROCEDURE  vectorNormInfinityReal8, vectorNormInfinityReal4
   END INTERFACE

   INTERFACE vectorSqrt
      MODULE PROCEDURE vectorSqrtReal8, vectorSqrtReal4
   END INTERFACE

   INTERFACE vectorSum
      MODULE PROCEDURE vectorSumReal8, vectorSumReal4
   END INTERFACE

   INTERFACE vectorMin
      MODULE PROCEDURE vectorMinReal8, vectorMinReal4, &
                       vectorMinInteger2, vectorMinInteger4, vectorMinInteger8
   END INTERFACE

   INTERFACE vectorMax
      MODULE PROCEDURE vectorMaxReal8, vectorMaxReal4, &
                       vectorMaxInteger2, vectorMaxInteger4, vectorMaxInteger8
   END INTERFACE

   INTERFACE vectorInsertValue
      MODULE PROCEDURE vectorInsertValueReal8, vectorInsertValueReal4, &
                       vectorInsertValueInteger2, vectorInsertValueInteger4, vectorInsertValueInteger8
   END INTERFACE

   INTERFACE vectorAddValue
      MODULE PROCEDURE vectorAddValueReal8, vectorAddValueReal4, &
                       vectorAddValueInteger2, vectorAddValueInteger4, vectorAddValueInteger8
   END INTERFACE

   INTERFACE vectorScale
      MODULE PROCEDURE vectorScaleReal8, vectorScaleReal4
   END INTERFACE

   INTERFACE vectorDot
      MODULE PROCEDURE vectorDotReal8, vectorDotReal4
   END INTERFACE

   INTERFACE vectorGetValue
      MODULE PROCEDURE vectorGetValueReal8, vectorGetValueReal4, &
                       vectorGetValueInteger2, vectorGetValueInteger4, vectorGetValueInteger8
   END INTERFACE

   INTERFACE vectorNorm
      MODULE PROCEDURE vectorNormReal8, vectorNormReal4
   END INTERFACE

   INTERFACE vectorGetValues
      MODULE PROCEDURE vectorGetValuesReal8, vectorGetValuesReal4, &
                       vectorGetValuesInteger2, vectorGetValuesInteger4, vectorGetValuesInteger8
   END INTERFACE

   INTERFACE vectorGetStartIndex
      MODULE PROCEDURE vectorGetStartIndexReal8, vectorGetStartIndexReal4, &
                       vectorGetStartIndexInteger2, vectorGetStartIndexInteger4, vectorGetStartIndexInteger8
   END INTERFACE

   INTERFACE vectorGetEndIndex
      MODULE PROCEDURE vectorGetEndIndexReal8, vectorGetEndIndexReal4, &
                       vectorGetEndIndexInteger2, vectorGetEndIndexInteger4, vectorGetEndIndexInteger8
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

! Procedure 2 : define the extra size for allocate vector
! --------------------------------------------------------
  SUBROUTINE vectorSetMemoryIncreaseSize(extraSize)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: extraSize

!     Body
!     - - -
      CALL mathSetMemoryIncreaseSize(extraSize)

  END SUBROUTINE

END MODULE vectorInterface

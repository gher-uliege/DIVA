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
   USE modulevectorInteger2Definition
   USE modulevectorInteger4Definition
   USE modulevectorInteger8Definition
   USE modulevectorReal4Definition
   USE modulevectorReal8Definition

   USE modulevectorInteger2, ONLY : &
                           arrayGetValuesInteger2 => arrayGetValues,  &
                           arrayGetValueInteger2 => arrayGetValue, &
                           arrayGetAllocationStatusInteger2 => arrayGetAllocationStatus, &
                           arrayGetPointerOnValueInteger2 => arrayGetPointerOnValue, &
                           arrayArraySetToZeroInteger2 => arrayArraySetToZero, &
                           arrayArraySetToValueInteger2 => arrayArraySetToValue, &
                           arrayArrayInsertValueInteger2 => arrayArrayInsertValue, &
                           arrayArrayAddValueInteger2 => arrayArrayAddValue, &
                           arrayArrayFastInsertValueInteger2 => arrayArrayFastInsertValue, &
                           arrayArrayFastAddValueInteger2 => arrayArrayFastAddValue, &
                           arraySetIncreaseSizeInteger2 => arraySetIncreaseSize, &
                           arrayDestructorInteger2 => arrayDestructor, &
                           arrayPrintInformationInteger2 => arrayPrintInformation, &
                           arrayCreateBaseInteger2 => arrayCreateBase, &
                           arrayCreateWithDimensionInteger2 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexInteger2 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeInteger2 => arraySetSize, &
                           arrayGetFirstIndexInteger2 => arrayGetFirstIndex, &
                           arrayGetLastIndexInteger2 => arrayGetLastIndex, &
                           arrayGetSizeInteger2 => arrayGetSize, &
                           arrayGetAllocatedSizeInteger2 => arrayGetAllocatedSize ,&
                           arrayGetIncreaseSizeInteger2 => arrayGetIncreaseSize, &
                           arrayGetFirstIndexXInteger2 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger2 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger2 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger2 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger2 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger2 => arraySetIncreaseSizeX, &
                           arrayArrayMinInteger2 => arrayArrayMin, arrayIOWriteInteger2 => arrayIOWrite, &
                           arrayArrayMaxInteger2 => arrayArrayMax, &
                           arrayArrayAbsMinInteger2 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger2 => arrayArrayAbsMax, arrayArrayPutInInteger2 => arrayArrayPutIn, &
                           arrayOptimizeInteger2 => arrayOptimize, arrayArraySetValueInteger2 => arrayArraySetValue

   USE modulevectorInteger4, ONLY : &
                           arrayGetValuesInteger4 => arrayGetValues,  &
                           arrayGetValueInteger4 => arrayGetValue, &
                           arrayGetAllocationStatusInteger4 => arrayGetAllocationStatus, &
                           arrayGetPointerOnValueInteger4 => arrayGetPointerOnValue, &
                           arrayArraySetToZeroInteger4 => arrayArraySetToZero, &
                           arrayArraySetToValueInteger4 => arrayArraySetToValue, &
                           arrayArrayInsertValueInteger4 => arrayArrayInsertValue, &
                           arrayArrayAddValueInteger4 => arrayArrayAddValue, &
                           arrayArrayFastInsertValueInteger4 => arrayArrayFastInsertValue, &
                           arrayArrayFastAddValueInteger4 => arrayArrayFastAddValue, &
                           arraySetIncreaseSizeInteger4 => arraySetIncreaseSize, &
                           arrayDestructorInteger4 => arrayDestructor, &
                           arrayPrintInformationInteger4 => arrayPrintInformation, &
                           arrayCreateBaseInteger4 => arrayCreateBase, &
                           arrayCreateWithDimensionInteger4 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexInteger4 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeInteger4 => arraySetSize, &
                           arrayGetFirstIndexInteger4 => arrayGetFirstIndex, &
                           arrayGetLastIndexInteger4 => arrayGetLastIndex, &
                           arrayGetSizeInteger4 => arrayGetSize, &
                           arrayGetAllocatedSizeInteger4 => arrayGetAllocatedSize ,&
                           arrayGetIncreaseSizeInteger4 => arrayGetIncreaseSize, &
                           arrayGetFirstIndexXInteger4 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger4 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger4 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger4 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger4 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger4 => arraySetIncreaseSizeX, &
                           arrayArrayMinInteger4 => arrayArrayMin, arrayIOWriteInteger4 => arrayIOWrite, &
                           arrayArrayMaxInteger4 => arrayArrayMax, &
                           arrayArrayAbsMinInteger4 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger4 => arrayArrayAbsMax, arrayArrayPutInInteger4 => arrayArrayPutIn, &
                           arrayOptimizeInteger4 => arrayOptimize, arrayArraySetValueInteger4 => arrayArraySetValue

   USE modulevectorInteger8, ONLY : &
                           arrayGetValuesInteger8 => arrayGetValues,  &
                           arrayGetValueInteger8 => arrayGetValue, &
                           arrayGetAllocationStatusInteger8 => arrayGetAllocationStatus, &
                           arrayGetPointerOnValueInteger8 => arrayGetPointerOnValue, &
                           arrayArraySetToZeroInteger8 => arrayArraySetToZero, &
                           arrayArraySetToValueInteger8 => arrayArraySetToValue, &
                           arrayArrayInsertValueInteger8 => arrayArrayInsertValue, &
                           arrayArrayAddValueInteger8 => arrayArrayAddValue, &
                           arrayArrayFastInsertValueInteger8 => arrayArrayFastInsertValue, &
                           arrayArrayFastAddValueInteger8 => arrayArrayFastAddValue, &
                           arraySetIncreaseSizeInteger8 => arraySetIncreaseSize, &
                           arrayDestructorInteger8 => arrayDestructor, &
                           arrayPrintInformationInteger8 => arrayPrintInformation, &
                           arrayCreateBaseInteger8 => arrayCreateBase, &
                           arrayCreateWithDimensionInteger8 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexInteger8 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeInteger8 => arraySetSize, &
                           arrayGetFirstIndexInteger8 => arrayGetFirstIndex, &
                           arrayGetLastIndexInteger8 => arrayGetLastIndex, &
                           arrayGetSizeInteger8 => arrayGetSize, &
                           arrayGetAllocatedSizeInteger8 => arrayGetAllocatedSize ,&
                           arrayGetIncreaseSizeInteger8 => arrayGetIncreaseSize, &
                           arrayGetFirstIndexXInteger8 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger8 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger8 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger8 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger8 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger8 => arraySetIncreaseSizeX, &
                           arrayArrayMinInteger8 => arrayArrayMin, arrayIOWriteInteger8 => arrayIOWrite, &
                           arrayArrayMaxInteger8 => arrayArrayMax, &
                           arrayArrayAbsMinInteger8 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger8 => arrayArrayAbsMax, arrayArrayPutInInteger8 => arrayArrayPutIn, &
                           arrayOptimizeInteger8 => arrayOptimize, arrayArraySetValueInteger8 => arrayArraySetValue

   USE modulevectorReal4, ONLY : &
                           arrayGetValuesReal4 => arrayGetValues,  &
                           arrayGetValueReal4 => arrayGetValue, &
                           arrayGetAllocationStatusReal4 => arrayGetAllocationStatus, &
                           arrayGetPointerOnValueReal4 => arrayGetPointerOnValue, &
                           arrayArraySetToZeroReal4 => arrayArraySetToZero, &
                           arrayArraySetToValueReal4 => arrayArraySetToValue, &
                           arrayArrayInsertValueReal4 => arrayArrayInsertValue, &
                           arrayArrayAddValueReal4 => arrayArrayAddValue, &
                           arrayArrayFastInsertValueReal4 => arrayArrayFastInsertValue, &
                           arrayArrayFastAddValueReal4 => arrayArrayFastAddValue, &
                           arraySetIncreaseSizeReal4 => arraySetIncreaseSize, &
                           arrayDestructorReal4 => arrayDestructor, &
                           arrayPrintInformationReal4 => arrayPrintInformation, &
                           arrayCreateBaseReal4 => arrayCreateBase, &
                           arrayCreateWithDimensionReal4 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexReal4 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeReal4 => arraySetSize, &
                           arrayGetFirstIndexReal4 => arrayGetFirstIndex, &
                           arrayGetLastIndexReal4 => arrayGetLastIndex, &
                           arrayGetSizeReal4 => arrayGetSize, &
                           arrayGetAllocatedSizeReal4 => arrayGetAllocatedSize ,&
                           arrayGetIncreaseSizeReal4 => arrayGetIncreaseSize, &
                           arrayGetFirstIndexXReal4 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXReal4 => arrayGetLastIndexX, &
                           arrayGetSizeXReal4 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXReal4 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXReal4 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXReal4 => arraySetIncreaseSizeX, &
                           arrayArrayMinReal4 => arrayArrayMin, arrayIOWriteReal4 => arrayIOWrite, &
                           arrayArrayMaxReal4 => arrayArrayMax, arrayIOReadReal4 =>  arrayIORead, &
                           arrayArrayNorm1Real4 => arrayArrayNorm1, &
                           arrayArrayNorm2Real4 => arrayArrayNorm2, &
                           arrayArrayNormInfinityReal4 => arrayArrayNormInfinity, &
                           arrayArrayNormReal4 => arrayArrayNorm, arrayArraySqrtReal4 => arrayArraySqrt, &
                           arrayArraySumReal4 => arrayArraySum, arrayArrayScaleReal4 => arrayArrayScale, &
                           arrayArrayDotReal4 => arrayArrayDot, arrayArrayAbsMinReal4 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxReal4 => arrayArrayAbsMax, arrayArrayPutInReal4 => arrayArrayPutIn, &
                           arrayOptimizeReal4 => arrayOptimize, arrayArraySetValueReal4 => arrayArraySetValue


   USE modulevectorReal8, ONLY : &
                           arrayGetValuesReal8 => arrayGetValues,  arrayGetValueReal8 => arrayGetValue, &
                           arrayGetAllocationStatusReal8 => arrayGetAllocationStatus, &
                           arrayGetPointerOnValueReal8 => arrayGetPointerOnValue, &
                           arrayArraySetToZeroReal8 => arrayArraySetToZero, &
                           arrayArraySetToValueReal8 => arrayArraySetToValue, &
                           arrayArrayInsertValueReal8 => arrayArrayInsertValue, &
                           arrayArrayAddValueReal8 => arrayArrayAddValue, &
                           arrayArrayFastInsertValueReal8 => arrayArrayFastInsertValue, &
                           arrayArrayFastAddValueReal8 => arrayArrayFastAddValue, &
                           arraySetIncreaseSizeReal8 => arraySetIncreaseSize, &
                           arrayDestructorReal8 => arrayDestructor, &
                           arrayPrintInformationReal8 => arrayPrintInformation, &
                           arrayCreateBaseReal8 => arrayCreateBase, &
                           arrayCreateWithDimensionReal8 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexReal8 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeReal8 => arraySetSize, &
                           arrayGetFirstIndexReal8 => arrayGetFirstIndex, &
                           arrayGetLastIndexReal8 => arrayGetLastIndex, &
                           arrayGetSizeReal8 => arrayGetSize, &
                           arrayGetAllocatedSizeReal8 => arrayGetAllocatedSize ,&
                           arrayGetIncreaseSizeReal8 => arrayGetIncreaseSize, &
                           arrayGetDefaultIncreaseSizeReal8 => arrayGetDefaultIncreaseSize, &
                           arrayGetFirstIndexXReal8 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXReal8 => arrayGetLastIndexX, &
                           arrayGetSizeXReal8 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXReal8 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXReal8 => arrayGetIncreaseSizeX, &
                           arrayGetDefaultIncreaseSizeXReal8 => arrayGetDefaultIncreaseSizeX, &
                           arraySetIncreaseSizeXReal8 => arraySetIncreaseSizeX, &
                           arrayArrayMinReal8 => arrayArrayMin, arrayIOWriteReal8 => arrayIOWrite, &
                           arrayArrayMaxReal8 => arrayArrayMax, arrayIOReadReal8 =>  arrayIORead, &
                           arrayArrayNorm1Real8 => arrayArrayNorm1, &
                           arrayArrayNorm2Real8 => arrayArrayNorm2, &
                           arrayArrayNormInfinityReal8 => arrayArrayNormInfinity, &
                           arrayArrayNormReal8 => arrayArrayNorm, arrayArrayPutInReal8 => arrayArrayPutIn, &
                           arrayArraySqrtReal8 => arrayArraySqrt, arrayArraySumReal8 => arrayArraySum, &
                           arrayArrayScaleReal8 => arrayArrayScale, arrayArrayDotReal8 => arrayArrayDot, &
                           arrayArrayAbsMinReal8 => arrayArrayAbsMin, arrayArrayAbsMaxReal8 => arrayArrayAbsMax, &
                           arrayOptimizeReal8 => arrayOptimize, arrayArraySetValueReal8 => arrayArraySetValue


! Interface
! =========
   INTERFACE vectorCreate
      MODULE PROCEDURE arrayCreateBaseReal8, arrayCreateWithDimensionReal8, arrayCreateWithDimensionAndFirstIndexReal8, &
                       arrayCreateBaseReal4, arrayCreateWithDimensionReal4, arrayCreateWithDimensionAndFirstIndexReal4, &
                   arrayCreateBaseInteger2, arrayCreateWithDimensionInteger2, arrayCreateWithDimensionAndFirstIndexInteger2, &
                   arrayCreateBaseInteger4, arrayCreateWithDimensionInteger4, arrayCreateWithDimensionAndFirstIndexInteger4, &
                   arrayCreateBaseInteger8, arrayCreateWithDimensionInteger8, arrayCreateWithDimensionAndFirstIndexInteger8
   END INTERFACE

   INTERFACE vectorPrint
      MODULE PROCEDURE arrayPrintInformationReal8, arrayPrintInformationReal4, &
                       arrayPrintInformationInteger2, arrayPrintInformationInteger4, arrayPrintInformationInteger8
   END INTERFACE

   INTERFACE vectorDestroy
      MODULE PROCEDURE arrayDestructorReal8, arrayDestructorReal4, &
                       arrayDestructorInteger2, arrayDestructorInteger4, arrayDestructorInteger8
   END INTERFACE

   INTERFACE vectorSetSize
      MODULE PROCEDURE arraySetSizeReal8, arraySetSizeReal4, &
                       arraySetSizeInteger2, arraySetSizeInteger4, arraySetSizeInteger8
   END INTERFACE

   INTERFACE vectorGetSize
      MODULE PROCEDURE arrayGetSizeReal8, arrayGetSizeReal4,&
                       arrayGetSizeInteger2, arrayGetSizeInteger4, arrayGetSizeInteger8
   END INTERFACE

   INTERFACE vectorGetSizeX
      MODULE PROCEDURE arrayGetSizeXReal8, arrayGetSizeXReal4,&
                       arrayGetSizeXInteger2, arrayGetSizeXInteger4, arrayGetSizeXInteger8
   END INTERFACE

   INTERFACE vectorSetToZero
      MODULE PROCEDURE arrayArraySetToZeroReal8, arrayArraySetToZeroReal4, &
                       arrayArraySetToZeroInteger2, arrayArraySetToZeroInteger4, arrayArraySetToZeroInteger8
   END INTERFACE

   INTERFACE vectorSetToValue
      MODULE PROCEDURE arrayArraySetToValueReal8, arrayArraySetToValueReal4, &
                       arrayArraySetToValueInteger2, arrayArraySetToValueInteger4, arrayArraySetToValueInteger8
   END INTERFACE

   INTERFACE vectorNorm1
      MODULE PROCEDURE arrayArrayNorm1Real8, arrayArrayNorm1Real4
   END INTERFACE

   INTERFACE vectorNorm2
      MODULE PROCEDURE arrayArrayNorm2Real8, arrayArrayNorm2Real4
   END INTERFACE

   INTERFACE vectorNormInfinity
      MODULE PROCEDURE  arrayArrayNormInfinityReal8, arrayArrayNormInfinityReal4
   END INTERFACE

   INTERFACE vectorSqrt
      MODULE PROCEDURE arrayArraySqrtReal8, arrayArraySqrtReal4
   END INTERFACE

   INTERFACE vectorSum
      MODULE PROCEDURE arrayArraySumReal8, arrayArraySumReal4
   END INTERFACE

   INTERFACE vectorMin
      MODULE PROCEDURE arrayArrayMinReal8, arrayArrayMinReal4, &
                       arrayArrayMinInteger2, arrayArrayMinInteger4, arrayArrayMinInteger8
   END INTERFACE

   INTERFACE vectorMax
      MODULE PROCEDURE arrayArrayMaxReal8, arrayArrayMaxReal4, &
                       arrayArrayMaxInteger2, arrayArrayMaxInteger4, arrayArrayMaxInteger8
   END INTERFACE

   INTERFACE vectorAbsMin
      MODULE PROCEDURE arrayArrayAbsMinReal8, arrayArrayAbsMinReal4, &
                       arrayArrayAbsMinInteger2, arrayArrayAbsMinInteger4, arrayArrayAbsMinInteger8
   END INTERFACE

   INTERFACE vectorAbsMax
      MODULE PROCEDURE arrayArrayAbsMaxReal8, arrayArrayAbsMaxReal4, &
                       arrayArrayAbsMaxInteger2, arrayArrayAbsMaxInteger4, arrayArrayAbsMaxInteger8
   END INTERFACE

   INTERFACE vectorInsertValue
      MODULE PROCEDURE arrayArrayInsertValueReal8, arrayArrayInsertValueReal4, &
                       arrayArrayInsertValueInteger2, arrayArrayInsertValueInteger4, arrayArrayInsertValueInteger8
   END INTERFACE

   INTERFACE vectorAddValue
      MODULE PROCEDURE arrayArrayAddValueReal8, arrayArrayAddValueReal4, &
                       arrayArrayAddValueInteger2, arrayArrayAddValueInteger4, arrayArrayAddValueInteger8
   END INTERFACE

   INTERFACE vectorFastInsertValue
      MODULE PROCEDURE arrayArrayFastInsertValueReal8, arrayArrayFastInsertValueReal4, &
                       arrayArrayFastInsertValueInteger2, arrayArrayFastInsertValueInteger4, arrayArrayFastInsertValueInteger8
   END INTERFACE

   INTERFACE vectorFastAddValue
      MODULE PROCEDURE arrayArrayFastAddValueReal8, arrayArrayFastAddValueReal4, &
                       arrayArrayFastAddValueInteger2, arrayArrayFastAddValueInteger4, arrayArrayFastAddValueInteger8
   END INTERFACE

   INTERFACE vectorScale
      MODULE PROCEDURE arrayArrayScaleReal8, arrayArrayScaleReal4
   END INTERFACE

   INTERFACE vectorDot
      MODULE PROCEDURE arrayArrayDotReal8, arrayArrayDotReal4
   END INTERFACE

   INTERFACE vectorGetValue
      MODULE PROCEDURE arrayGetValueReal8, arrayGetValueReal4, &
                       arrayGetValueInteger2, arrayGetValueInteger4, arrayGetValueInteger8
   END INTERFACE

   INTERFACE vectorNorm
      MODULE PROCEDURE arrayArrayNormReal8, arrayArrayNormReal4
   END INTERFACE

   INTERFACE vectorGetValues
      MODULE PROCEDURE arrayGetValuesReal8, arrayGetValuesReal4, &
                       arrayGetValuesInteger2, arrayGetValuesInteger4, arrayGetValuesInteger8
   END INTERFACE

   INTERFACE vectorSetIncreaseSize
      MODULE PROCEDURE arraySetIncreaseSizeReal8, arraySetIncreaseSizeReal4, &
                       arraySetIncreaseSizeInteger2, arraySetIncreaseSizeInteger4, arraySetIncreaseSizeInteger8
   END INTERFACE

   INTERFACE vectorGetFirstIndex
      MODULE PROCEDURE arrayGetFirstIndexReal8, arrayGetFirstIndexReal4, &
                       arrayGetFirstIndexInteger2, arrayGetFirstIndexInteger4, arrayGetFirstIndexInteger8
   END INTERFACE

   INTERFACE vectorGetFirstIndexX
      MODULE PROCEDURE arrayGetFirstIndexXReal8, arrayGetFirstIndexXReal4, &
                       arrayGetFirstIndexXInteger2, arrayGetFirstIndexXInteger4, arrayGetFirstIndexXInteger8
   END INTERFACE

   INTERFACE vectorGetLastIndex
      MODULE PROCEDURE arrayGetLastIndexReal8, arrayGetLastIndexReal4, &
                       arrayGetLastIndexInteger2, arrayGetLastIndexInteger4, arrayGetLastIndexInteger8
   END INTERFACE

   INTERFACE vectorGetLastIndexX
      MODULE PROCEDURE arrayGetLastIndexXReal8, arrayGetLastIndexXReal4, &
                       arrayGetLastIndexXInteger2, arrayGetLastIndexXInteger4, arrayGetLastIndexXInteger8
   END INTERFACE

   INTERFACE vectorGetAllocationStatus
      MODULE PROCEDURE arrayGetAllocationStatusReal8, arrayGetAllocationStatusReal4, &
                       arrayGetAllocationStatusInteger2, arrayGetAllocationStatusInteger4, arrayGetAllocationStatusInteger8
   END INTERFACE

   INTERFACE vectorGetAllocatedSize
      MODULE PROCEDURE arrayGetAllocatedSizeReal8, arrayGetAllocatedSizeReal4, &
                       arrayGetAllocatedSizeInteger2, arrayGetAllocatedSizeInteger4, arrayGetAllocatedSizeInteger8
   END INTERFACE

   INTERFACE vectorGetAllocatedSizeX
      MODULE PROCEDURE arrayGetAllocatedSizeXReal8, arrayGetAllocatedSizeXReal4, &
                       arrayGetAllocatedSizeXInteger2, arrayGetAllocatedSizeXInteger4, arrayGetAllocatedSizeXInteger8
   END INTERFACE

   INTERFACE vectorGetPointerOnValue
      MODULE PROCEDURE arrayGetPointerOnValueReal8, arrayGetPointerOnValueReal4, &
                       arrayGetPointerOnValueInteger2, arrayGetPointerOnValueInteger4, arrayGetPointerOnValueInteger8
   END INTERFACE

   INTERFACE vectorGetIncreaseSize
      MODULE PROCEDURE arrayGetIncreaseSizeReal8, arrayGetIncreaseSizeReal4, &
                       arrayGetIncreaseSizeInteger2, arrayGetIncreaseSizeInteger4, arrayGetIncreaseSizeInteger8
   END INTERFACE

   INTERFACE vectorGetDefaultIncreaseSize
      MODULE PROCEDURE arrayGetDefaultIncreaseSizeReal8
   END INTERFACE

   INTERFACE vectorGetDefaultIncreaseSizeX
      MODULE PROCEDURE arrayGetDefaultIncreaseSizeXReal8
   END INTERFACE

   INTERFACE vectorGetIncreaseSizeX
      MODULE PROCEDURE arrayGetIncreaseSizeXReal8, arrayGetIncreaseSizeXReal4, &
                       arrayGetIncreaseSizeXInteger2, arrayGetIncreaseSizeXInteger4, arrayGetIncreaseSizeXInteger8
   END INTERFACE

   INTERFACE vectorSetIncreaseSizeX
      MODULE PROCEDURE arraySetIncreaseSizeXReal8, arraySetIncreaseSizeXReal4, &
                       arraySetIncreaseSizeXInteger2, arraySetIncreaseSizeXInteger4, arraySetIncreaseSizeXInteger8
   END INTERFACE

   INTERFACE vectorOptimize
      MODULE PROCEDURE arrayOptimizeReal8, arrayOptimizeReal4, &
                       arrayOptimizeInteger2, arrayOptimizeInteger4, arrayOptimizeInteger8
   END INTERFACE

   INTERFACE vectorSetValue
      MODULE PROCEDURE arrayArraySetValueReal8, arrayArraySetValueReal4, &
                       arrayArraySetValueInteger2,arrayArraySetValueInteger4, arrayArraySetValueInteger8
   END INTERFACE

   INTERFACE vectorPutIn
      MODULE PROCEDURE arrayArrayPutInReal8, arrayArrayPutInReal4, &
                       arrayArrayPutInInteger2,arrayArrayPutInInteger4, arrayArrayPutInInteger8
   END INTERFACE

   INTERFACE vectorWrite
      MODULE PROCEDURE arrayIOWriteReal4, arrayIOWriteReal8, &
                       arrayIOWriteInteger2, arrayIOWriteInteger4, arrayIOWriteInteger8
   END INTERFACE

   INTERFACE vectorRead
      MODULE PROCEDURE arrayIOReadReal4, arrayIOReadReal8
   END INTERFACE

END MODULE vectorInterface

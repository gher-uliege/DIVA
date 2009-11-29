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
   USE modulematrixInteger2Definition
   USE modulematrixInteger4Definition
   USE modulematrixInteger8Definition
   USE modulematrixReal4Definition
   USE modulematrixReal8Definition

   USE modulematrixInteger2, ONLY : &
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
                           arrayGetFirstIndexXInteger2 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger2 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger2 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger2 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger2 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger2 => arraySetIncreaseSizeX, &
                           arrayGetFirstIndexYInteger2 => arrayGetFirstIndexY, &
                           arrayGetLastIndexYInteger2 => arrayGetLastIndexY, &
                           arrayGetSizeYInteger2 => arrayGetSizeY, &
                           arrayGetAllocatedSizeYInteger2 => arrayGetAllocatedSizeY , &
                           arrayGetIncreaseSizeYInteger2 => arrayGetIncreaseSizeY, &
                           arraySetIncreaseSizeYInteger2 => arraySetIncreaseSizeY, &
                           arrayArrayMinInteger2 => arrayArrayMin, arrayIOWriteInteger2 => arrayIOWrite, &
                           arrayArrayMaxInteger2 => arrayArrayMax, &
                           arrayArrayAbsMinInteger2 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger2 => arrayArrayAbsMax, &
                           arrayOptimizeInteger2 => arrayOptimize, arrayArraySetValueInteger2 => arrayArraySetValue

   USE modulematrixInteger4, ONLY : &
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
                           arrayGetFirstIndexXInteger4 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger4 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger4 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger4 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger4 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger4 => arraySetIncreaseSizeX, &
                           arrayGetFirstIndexYInteger4 => arrayGetFirstIndexY, &
                           arrayGetLastIndexYInteger4 => arrayGetLastIndexY, &
                           arrayGetSizeYInteger4 => arrayGetSizeY, &
                           arrayGetAllocatedSizeYInteger4 => arrayGetAllocatedSizeY , &
                           arrayGetIncreaseSizeYInteger4 => arrayGetIncreaseSizeY, &
                           arraySetIncreaseSizeYInteger4 => arraySetIncreaseSizeY, &
                           arrayArrayMinInteger4 => arrayArrayMin, arrayIOWriteInteger4 => arrayIOWrite, &
                           arrayArrayMaxInteger4 => arrayArrayMax, &
                           arrayArrayAbsMinInteger4 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger4 => arrayArrayAbsMax, &
                           arrayOptimizeInteger4 => arrayOptimize, arrayArraySetValueInteger4 => arrayArraySetValue

   USE modulematrixInteger8, ONLY : &
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
                           arrayGetFirstIndexXInteger8 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXInteger8 => arrayGetLastIndexX, &
                           arrayGetSizeXInteger8 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXInteger8 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXInteger8 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXInteger8 => arraySetIncreaseSizeX, &
                           arrayGetFirstIndexYInteger8 => arrayGetFirstIndexY, &
                           arrayGetLastIndexYInteger8 => arrayGetLastIndexY, &
                           arrayGetSizeYInteger8 => arrayGetSizeY, &
                           arrayGetAllocatedSizeYInteger8 => arrayGetAllocatedSizeY , &
                           arrayGetIncreaseSizeYInteger8 => arrayGetIncreaseSizeY, &
                           arraySetIncreaseSizeYInteger8 => arraySetIncreaseSizeY, &
                           arrayArrayMinInteger8 => arrayArrayMin, arrayIOWriteInteger8 => arrayIOWrite, &
                           arrayArrayMaxInteger8 => arrayArrayMax, &
                           arrayArrayAbsMinInteger8 => arrayArrayAbsMin, &
                           arrayArrayAbsMaxInteger8 => arrayArrayAbsMax, &
                           arrayOptimizeInteger8 => arrayOptimize, arrayArraySetValueInteger8 => arrayArraySetValue

   USE modulematrixReal4, ONLY : &
                           arrayGetValuesReal4 => arrayGetValues, arrayGetValueReal4 => arrayGetValue, &
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
                           arraySetSizeReal4 => arraySetSize, arrayGetFirstIndexXReal4 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXReal4 => arrayGetLastIndexX, arrayGetSizeXReal4 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXReal4 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXReal4 => arrayGetIncreaseSizeX, &
                           arraySetIncreaseSizeXReal4 => arraySetIncreaseSizeX, &
                           arrayGetFirstIndexYReal4 => arrayGetFirstIndexY, &
                           arrayGetLastIndexYReal4 => arrayGetLastIndexY, &
                           arrayGetSizeYReal4 => arrayGetSizeY, &
                           arrayGetAllocatedSizeYReal4 => arrayGetAllocatedSizeY , &
                           arrayGetIncreaseSizeYReal4 => arrayGetIncreaseSizeY, &
                           arraySetIncreaseSizeYReal4 => arraySetIncreaseSizeY, &
                           arrayArrayMinReal4 => arrayArrayMin, &
                           arrayArrayMaxReal4 => arrayArrayMax, &
                           arrayArrayAbsMinReal4 => arrayArrayAbsMin, arrayIOWriteReal4 => arrayIOWrite, &
                           arrayArrayAbsMaxReal4 => arrayArrayAbsMax, arrayIOReadReal4 =>  arrayIORead, &
                           arrayArrayNorm1Real4 => arrayArrayNorm1, &
                           arrayArrayNorm2Real4 => arrayArrayNorm2, &
                           arrayArrayNormInfinityReal4 => arrayArrayNormInfinity, &
                           arrayArrayNormReal4 => arrayArrayNorm, &
                           arrayArraySqrtReal4 => arrayArraySqrt, &
                           arrayArraySumReal4 => arrayArraySum, &
                           arrayArrayScaleReal4 => arrayArrayScale, &
                           arrayOptimizeReal4 => arrayOptimize, arrayArraySetValueReal4 => arrayArraySetValue

   USE modulematrixReal8, ONLY : &
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
                           arrayDestructorReal8 => arrayDestructor, arrayPrintInformationReal8 => arrayPrintInformation, &
                           arrayCreateBaseReal8 => arrayCreateBase, arrayCreateWithDimensionReal8 => arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndexReal8 => arrayCreateWithDimensionAndFirstIndex, &
                           arraySetSizeReal8 => arraySetSize, arrayGetFirstIndexXReal8 => arrayGetFirstIndexX, &
                           arrayGetLastIndexXReal8 => arrayGetLastIndexX, arrayGetSizeXReal8 => arrayGetSizeX, &
                           arrayGetAllocatedSizeXReal8 => arrayGetAllocatedSizeX , &
                           arrayGetIncreaseSizeXReal8 => arrayGetIncreaseSizeX, &
                           arrayGetDefaultIncreaseSizeXReal8 => arrayGetDefaultIncreaseSizeX, &
                           arraySetIncreaseSizeXReal8 => arraySetIncreaseSizeX, &
                           arrayGetFirstIndexYReal8 => arrayGetFirstIndexY, &
                           arrayGetLastIndexYReal8 => arrayGetLastIndexY, &
                           arrayGetSizeYReal8 => arrayGetSizeY, arrayGetAllocatedSizeYReal8 => arrayGetAllocatedSizeY , &
                           arrayGetIncreaseSizeYReal8 => arrayGetIncreaseSizeY, &
                           arrayGetDefaultIncreaseSizeYReal8 => arrayGetDefaultIncreaseSizeY, &
                           arraySetIncreaseSizeYReal8 => arraySetIncreaseSizeY, &
                           arrayArrayMinReal8 => arrayArrayMin, arrayArrayMaxReal8 => arrayArrayMax, &
                           arrayArrayAbsMinReal8 => arrayArrayAbsMin, arrayIOWriteReal8 => arrayIOWrite, &
                           arrayArrayAbsMaxReal8 => arrayArrayAbsMax, arrayIOReadReal8 =>  arrayIORead, &
                           arrayArrayNorm1Real8 => arrayArrayNorm1, &
                           arrayArrayNorm2Real8 => arrayArrayNorm2, &
                           arrayArrayNormInfinityReal8 => arrayArrayNormInfinity, &
                           arrayArrayNormReal8 => arrayArrayNorm, &
                           arrayArraySqrtReal8 => arrayArraySqrt, &
                           arrayArraySumReal8 => arrayArraySum, &
                           arrayArrayScaleReal8 => arrayArrayScale, &
                           arrayOptimizeReal8 => arrayOptimize, arrayArraySetValueReal8 => arrayArraySetValue


! Interface
! =========
   INTERFACE matrixCreate
      MODULE PROCEDURE arrayCreateBaseReal8, arrayCreateWithDimensionReal8, arrayCreateWithDimensionAndFirstIndexReal8, &
                       arrayCreateBaseReal4, arrayCreateWithDimensionReal4, arrayCreateWithDimensionAndFirstIndexReal4, &
                   arrayCreateBaseInteger2, arrayCreateWithDimensionInteger2, arrayCreateWithDimensionAndFirstIndexInteger2, &
                   arrayCreateBaseInteger4, arrayCreateWithDimensionInteger4, arrayCreateWithDimensionAndFirstIndexInteger4, &
                   arrayCreateBaseInteger8, arrayCreateWithDimensionInteger8, arrayCreateWithDimensionAndFirstIndexInteger8
   END INTERFACE

   INTERFACE matrixPrint
      MODULE PROCEDURE arrayPrintInformationReal8, arrayPrintInformationReal4, &
                       arrayPrintInformationInteger2, arrayPrintInformationInteger4, arrayPrintInformationInteger8
   END INTERFACE

   INTERFACE matrixDestroy
      MODULE PROCEDURE arrayDestructorReal8, arrayDestructorReal4, &
                       arrayDestructorInteger2, arrayDestructorInteger4, arrayDestructorInteger8
   END INTERFACE

   INTERFACE matrixSetSize
      MODULE PROCEDURE arraySetSizeReal8, arraySetSizeReal4, &
                       arraySetSizeInteger2, arraySetSizeInteger4, arraySetSizeInteger8
   END INTERFACE

   INTERFACE matrixGetSizeX
      MODULE PROCEDURE arrayGetSizeXReal8, arrayGetSizeXReal4,&
                       arrayGetSizeXInteger2, arrayGetSizeXInteger4, arrayGetSizeXInteger8
   END INTERFACE

   INTERFACE matrixGetSizeY
      MODULE PROCEDURE arrayGetSizeYReal8, arrayGetSizeYReal4,&
                       arrayGetSizeYInteger2, arrayGetSizeYInteger4, arrayGetSizeYInteger8
   END INTERFACE

   INTERFACE matrixSetToZero
      MODULE PROCEDURE arrayArraySetToZeroReal8, arrayArraySetToZeroReal4, &
                       arrayArraySetToZeroInteger2, arrayArraySetToZeroInteger4, arrayArraySetToZeroInteger8
   END INTERFACE

   INTERFACE matrixSetToValue
      MODULE PROCEDURE arrayArraySetToValueReal8, arrayArraySetToValueReal4, &
                       arrayArraySetToValueInteger2, arrayArraySetToValueInteger4, arrayArraySetToValueInteger8
   END INTERFACE

   INTERFACE matrixNorm1
      MODULE PROCEDURE arrayArrayNorm1Real8, arrayArrayNorm1Real4
   END INTERFACE

   INTERFACE matrixNorm2
      MODULE PROCEDURE arrayArrayNorm2Real8, arrayArrayNorm2Real4
   END INTERFACE

   INTERFACE matrixNormInfinity
      MODULE PROCEDURE  arrayArrayNormInfinityReal8, arrayArrayNormInfinityReal4
   END INTERFACE

   INTERFACE matrixSqrt
      MODULE PROCEDURE arrayArraySqrtReal8, arrayArraySqrtReal4
   END INTERFACE

   INTERFACE matrixSum
      MODULE PROCEDURE arrayArraySumReal8, arrayArraySumReal4
   END INTERFACE

   INTERFACE matrixMin
      MODULE PROCEDURE arrayArrayMinReal8, arrayArrayMinReal4, &
                       arrayArrayMinInteger2, arrayArrayMinInteger4, arrayArrayMinInteger8
   END INTERFACE

   INTERFACE matrixMax
      MODULE PROCEDURE arrayArrayMaxReal8, arrayArrayMaxReal4, &
                       arrayArrayMaxInteger2, arrayArrayMaxInteger4, arrayArrayMaxInteger8
   END INTERFACE

   INTERFACE matrixAbsMin
      MODULE PROCEDURE arrayArrayAbsMinReal8, arrayArrayAbsMinReal4, &
                       arrayArrayAbsMinInteger2, arrayArrayAbsMinInteger4, arrayArrayAbsMinInteger8
   END INTERFACE

   INTERFACE matrixAbsMax
      MODULE PROCEDURE arrayArrayAbsMaxReal8, arrayArrayAbsMaxReal4, &
                       arrayArrayAbsMaxInteger2, arrayArrayAbsMaxInteger4, arrayArrayAbsMaxInteger8
   END INTERFACE

   INTERFACE matrixInsertValue
      MODULE PROCEDURE arrayArrayInsertValueReal8, arrayArrayInsertValueReal4, &
                       arrayArrayInsertValueInteger2, arrayArrayInsertValueInteger4, arrayArrayInsertValueInteger8
   END INTERFACE

   INTERFACE matrixAddValue
      MODULE PROCEDURE arrayArrayAddValueReal8, arrayArrayAddValueReal4, &
                       arrayArrayAddValueInteger2, arrayArrayAddValueInteger4, arrayArrayAddValueInteger8
   END INTERFACE

   INTERFACE matrixFastInsertValue
      MODULE PROCEDURE arrayArrayFastInsertValueReal8, arrayArrayFastInsertValueReal4, &
                       arrayArrayFastInsertValueInteger2, arrayArrayFastInsertValueInteger4, arrayArrayFastInsertValueInteger8
   END INTERFACE

   INTERFACE matrixFastAddValue
      MODULE PROCEDURE arrayArrayFastAddValueReal8, arrayArrayFastAddValueReal4, &
                       arrayArrayFastAddValueInteger2, arrayArrayFastAddValueInteger4, arrayArrayFastAddValueInteger8
   END INTERFACE

   INTERFACE matrixScale
      MODULE PROCEDURE arrayArrayScaleReal8, arrayArrayScaleReal4
   END INTERFACE

   INTERFACE matrixGetValue
      MODULE PROCEDURE arrayGetValueReal8, arrayGetValueReal4, &
                       arrayGetValueInteger2, arrayGetValueInteger4, arrayGetValueInteger8
   END INTERFACE

   INTERFACE matrixNorm
      MODULE PROCEDURE arrayArrayNormReal8, arrayArrayNormReal4
   END INTERFACE

   INTERFACE matrixGetValues
      MODULE PROCEDURE arrayGetValuesReal8, arrayGetValuesReal4, &
                       arrayGetValuesInteger2, arrayGetValuesInteger4, arrayGetValuesInteger8
   END INTERFACE

   INTERFACE matrixGetFirstIndexX
      MODULE PROCEDURE arrayGetFirstIndexXReal8, arrayGetFirstIndexXReal4, &
                       arrayGetFirstIndexXInteger2, arrayGetFirstIndexXInteger4, arrayGetFirstIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetFirstIndexY
      MODULE PROCEDURE arrayGetFirstIndexYReal8, arrayGetFirstIndexYReal4, &
                       arrayGetFirstIndexYInteger2, arrayGetFirstIndexYInteger4, arrayGetFirstIndexYInteger8
   END INTERFACE

   INTERFACE matrixGetLastIndexX
      MODULE PROCEDURE arrayGetLastIndexXReal8, arrayGetLastIndexXReal4, &
                       arrayGetLastIndexXInteger2, arrayGetLastIndexXInteger4, arrayGetLastIndexXInteger8
   END INTERFACE

   INTERFACE matrixGetLastIndexY
      MODULE PROCEDURE arrayGetLastIndexYReal8, arrayGetLastIndexYReal4, &
                       arrayGetLastIndexYInteger2, arrayGetLastIndexYInteger4, arrayGetLastIndexYInteger8
   END INTERFACE

   INTERFACE matrixGetAllocationStatus
      MODULE PROCEDURE arrayGetAllocationStatusReal8, arrayGetAllocationStatusReal4, &
                       arrayGetAllocationStatusInteger2, arrayGetAllocationStatusInteger4, arrayGetAllocationStatusInteger8
   END INTERFACE

   INTERFACE matrixGetAllocatedSizeX
      MODULE PROCEDURE arrayGetAllocatedSizeXReal8, arrayGetAllocatedSizeXReal4, &
                       arrayGetAllocatedSizeXInteger2, arrayGetAllocatedSizeXInteger4, arrayGetAllocatedSizeXInteger8
   END INTERFACE

   INTERFACE matrixGetAllocatedSizeY
      MODULE PROCEDURE arrayGetAllocatedSizeYReal8, arrayGetAllocatedSizeYReal4, &
                       arrayGetAllocatedSizeYInteger2, arrayGetAllocatedSizeYInteger4, arrayGetAllocatedSizeYInteger8
   END INTERFACE

   INTERFACE matrixGetPointerOnValue
      MODULE PROCEDURE arrayGetPointerOnValueReal8, arrayGetPointerOnValueReal4, &
                       arrayGetPointerOnValueInteger2, arrayGetPointerOnValueInteger4, arrayGetPointerOnValueInteger8
   END INTERFACE

   INTERFACE matrixGetDefaultIncreaseSizeX
      MODULE PROCEDURE arrayGetDefaultIncreaseSizeXReal8
   END INTERFACE

   INTERFACE matrixGetIncreaseSizeX
      MODULE PROCEDURE arrayGetIncreaseSizeXReal8, arrayGetIncreaseSizeXReal4, &
                       arrayGetIncreaseSizeXInteger2, arrayGetIncreaseSizeXInteger4, arrayGetIncreaseSizeXInteger8
   END INTERFACE

   INTERFACE matrixSetIncreaseSizeX
      MODULE PROCEDURE arraySetIncreaseSizeXReal8, arraySetIncreaseSizeXReal4, &
                       arraySetIncreaseSizeXInteger2, arraySetIncreaseSizeXInteger4, arraySetIncreaseSizeXInteger8
   END INTERFACE

   INTERFACE matrixGetDefaultIncreaseSizeY
      MODULE PROCEDURE arrayGetDefaultIncreaseSizeYReal8
   END INTERFACE

   INTERFACE matrixGetIncreaseSizeY
      MODULE PROCEDURE arrayGetIncreaseSizeYReal8, arrayGetIncreaseSizeYReal4, &
                       arrayGetIncreaseSizeYInteger2, arrayGetIncreaseSizeYInteger4, arrayGetIncreaseSizeYInteger8
   END INTERFACE

   INTERFACE matrixSetIncreaseSizeY
      MODULE PROCEDURE arraySetIncreaseSizeYReal8, arraySetIncreaseSizeYReal4, &
                       arraySetIncreaseSizeYInteger2, arraySetIncreaseSizeYInteger4, arraySetIncreaseSizeYInteger8
   END INTERFACE

   INTERFACE matrixSetIncreaseSize
      MODULE PROCEDURE arraySetIncreaseSizeReal8, arraySetIncreaseSizeReal4, &
                       arraySetIncreaseSizeInteger2, arraySetIncreaseSizeInteger4, arraySetIncreaseSizeInteger8
   END INTERFACE

   INTERFACE matrixOptimize
      MODULE PROCEDURE arrayOptimizeReal8, arrayOptimizeReal4, &
                       arrayOptimizeInteger2, arrayOptimizeInteger4, arrayOptimizeInteger8
   END INTERFACE

   INTERFACE matrixSetValue
      MODULE PROCEDURE arrayArraySetValueReal8, arrayArraySetValueReal4, &
                       arrayArraySetValueInteger2,arrayArraySetValueInteger4, arrayArraySetValueInteger8
   END INTERFACE

   INTERFACE matrixWrite
      MODULE PROCEDURE arrayIOWriteReal4, arrayIOWriteReal8, &
                       arrayIOWriteInteger2, arrayIOWriteInteger4, arrayIOWriteInteger8
   END INTERFACE

   INTERFACE matrixRead
      MODULE PROCEDURE arrayIOReadReal4, arrayIOReadReal8
   END INTERFACE

END MODULE matrixInterface

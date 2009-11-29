MODULE moduleArray3D

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

! Preprocessing declaration
! =========================

! Include file
! ============
   USE moduleArray, ONLY : arrayGetValues, arrayGetValue, arrayGetAllocationStatus, arrayGetPointerOnValue, &
                           arrayArraySetToZero, arrayArraySetToValue, arrayArrayInsertValue, arrayArrayAddValue, &
                           arrayArrayFastInsertValue, arrayArrayFastAddValue, arraySetIncreaseSize, &
                           arrayDestructor, arrayPrintInformation, arrayCreateBase, arrayCreateWithDimension, &
                           arrayCreateWithDimensionAndFirstIndex, arraySetSize, &
                           arrayGetFirstIndexX, arrayGetLastIndexX, arrayGetSizeX, arrayGetAllocatedSizeX , arrayGetIncreaseSizeX, &
                           arrayGetDefaultIncreaseSizeX, arraySetIncreaseSizeX, &
                           arrayGetFirstIndexY, arrayGetLastIndexY, arrayGetSizeY, arrayGetAllocatedSizeY , arrayGetIncreaseSizeY, &
                           arrayGetDefaultIncreaseSizeY, arraySetIncreaseSizeY, &
                           arrayGetFirstIndexZ, arrayGetLastIndexZ, arrayGetSizeZ, arrayGetAllocatedSizeZ , arrayGetIncreaseSizeZ, &
                           arrayGetDefaultIncreaseSizeZ, arraySetIncreaseSizeZ, &
                           arrayArrayMin, arrayArrayMax, arrayOptimize, arrayArraySetValue, &
#ifdef _REAL_
                           arrayArrayNorm1, arrayArrayNorm2, arrayArrayNormInfinity, arrayArrayNorm, arrayArraySqrt, &
                           arrayArraySum, arrayArrayScale, &
                           arrayIOWrite, arrayIORead, &
#endif
                           arrayArrayAbsMin, arrayArrayAbsMax


! Procedures status
! =================

!  General part
!  ------------

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

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================


END MODULE moduleArray3D


MODULE moduleMatrix

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
                           arrayArrayMin, arrayArrayMax, arrayOptimize, arrayArraySetValue, &
#ifdef _REAL_
                           arrayArrayNorm1, arrayArrayNorm2, arrayArrayNormInfinity, arrayArrayNorm, arrayArraySqrt, &
                           arrayArraySum, arrayArrayScale, &
                           arrayIORead, &
#endif
                           arrayIOWrite, &
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



END MODULE moduleMatrix


MODULE template

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
   USE templatearrayType
   USE templateBasicArray, ONLY : &
             printInformation, arrayDestroy, arraySetSize, arrayGetSizeX, arrayGetSizeY, arrayGetSizeZ, &
             arraySetToZero, arraySetToValue, arrayMin, arrayMax, arrayInsertValue, arrayAddValue, arrayGetValue, &
             arrayCreateBase, arrayCreateWithDimension, arrayCreateWithDimensionAndStartingPoint, arrayGetValues, &
             arraySetIncreaseSize, &
             arrayGetStartIndexX, arrayGetEndIndexX, arrayGetStartIndexY, arrayGetEndIndexY, arrayGetStartIndexZ, arrayGetEndIndexZ


END MODULE template


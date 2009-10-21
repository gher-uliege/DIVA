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
             printInformation, arrayDestroy, arraySetSize, arrayGetSizeX, arrayGetSizeY, arraySetToZero, arraySetToValue, &
             arrayMin, arrayMax, arrayInsertValue, arrayAddValue, arrayGetValue, &
             arrayCreateBase, arrayCreateWithDimension, arrayCreateWithDimensionAndStartingPoint, arrayGetValues, &
             arrayGetStartIndexX, arrayGetEndIndexX, arrayGetStartIndexY, arrayGetEndIndexY


END MODULE template


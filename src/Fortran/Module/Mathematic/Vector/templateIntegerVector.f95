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
   USE templatevectorType
   USE templateBasicVector, ONLY : &
             printInformation, vectorDestroy, vectorSetSize, vectorGetSize, vectorSetToZero, vectorSetToValue, &
             vectorMin, vectorMax, vectorInsertValue, vectorAddValue, vectorGetValue, vectorSetIncreaseSize, &
             vectorCreateBase, vectorCreateWithDimension, vectorCreateWithDimensionAndStartingPoint, vectorGetValues, &
             vectorGetStartIndex, vectorGetEndIndex, vectorAbsMin, vectorAbsMax


END MODULE template


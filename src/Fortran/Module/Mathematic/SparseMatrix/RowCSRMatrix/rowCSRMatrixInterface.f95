MODULE rowCSRMatrixInterface
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
   USE modulerowcsrmatrixInteger2Definition
   USE modulerowcsrmatrixInteger4Definition
   USE modulerowcsrmatrixInteger8Definition
   USE modulerowcsrmatrixReal4Definition
   USE modulerowcsrmatrixReal8Definition

   USE modulerowcsrmatrixInteger2, ONLY : rowCSRMatrixGetValuesInteger2 => rowCSRMatrixGetValues, &
                                          rowCSRMatrixGetIndexInteger2 => rowCSRMatrixGetIndex, &
                                          rowCSRMatrixGetValueInteger2 => rowCSRMatrixGetValue, &
                                          rowCSRMatrixGetPointerOnValueInteger2 => rowCSRMatrixGetPointerOnValue, &
                                          rowCSRMatrixSetToZeroInteger2 => rowCSRMatrixSetToZero, &
                                          rowCSRMatrixSetToValueInteger2 => rowCSRMatrixSetToValue, &
                                          rowCSRMatrixInsertValueInteger2 => rowCSRMatrixInsertValue, &
                                          rowCSRMatrixAddValueInteger2 => rowCSRMatrixAddValue

   USE modulerowcsrmatrixInteger4, ONLY : rowCSRMatrixGetValuesInteger4 => rowCSRMatrixGetValues, &
                                          rowCSRMatrixGetIndexInteger4 => rowCSRMatrixGetIndex, &
                                          rowCSRMatrixGetValueInteger4 => rowCSRMatrixGetValue, &
                                          rowCSRMatrixGetPointerOnValueInteger4 => rowCSRMatrixGetPointerOnValue, &
                                          rowCSRMatrixSetToZeroInteger4 => rowCSRMatrixSetToZero, &
                                          rowCSRMatrixSetToValueInteger4 => rowCSRMatrixSetToValue, &
                                          rowCSRMatrixInsertValueInteger4 => rowCSRMatrixInsertValue, &
                                          rowCSRMatrixAddValueInteger4 => rowCSRMatrixAddValue

   USE modulerowcsrmatrixInteger8, ONLY : rowCSRMatrixGetValuesInteger8 => rowCSRMatrixGetValues, &
                                          rowCSRMatrixGetIndexInteger8 => rowCSRMatrixGetIndex, &
                                          rowCSRMatrixGetValueInteger8 => rowCSRMatrixGetValue, &
                                          rowCSRMatrixGetPointerOnValueInteger8 => rowCSRMatrixGetPointerOnValue, &
                                          rowCSRMatrixSetToZeroInteger8 => rowCSRMatrixSetToZero, &
                                          rowCSRMatrixSetToValueInteger8 => rowCSRMatrixSetToValue, &
                                          rowCSRMatrixInsertValueInteger8 => rowCSRMatrixInsertValue, &
                                          rowCSRMatrixAddValueInteger8 => rowCSRMatrixAddValue

   USE modulerowcsrmatrixReal4, ONLY : rowCSRMatrixGetValuesReal4 => rowCSRMatrixGetValues, &
                                       rowCSRMatrixGetIndexReal4 => rowCSRMatrixGetIndex, &
                                       rowCSRMatrixGetValueReal4 => rowCSRMatrixGetValue, &
                                       rowCSRMatrixGetPointerOnValueReal4 => rowCSRMatrixGetPointerOnValue, &
                                       rowCSRMatrixSetToZeroReal4 => rowCSRMatrixSetToZero, &
                                       rowCSRMatrixSetToValueReal4 => rowCSRMatrixSetToValue, &
                                       rowCSRMatrixInsertValueReal4 => rowCSRMatrixInsertValue, &
                                       rowCSRMatrixAddValueReal4 => rowCSRMatrixAddValue

   USE modulerowcsrmatrixReal8, ONLY : rowCSRMatrixGetValuesReal8 => rowCSRMatrixGetValues, &
                                       rowCSRMatrixGetIndexReal8 => rowCSRMatrixGetIndex, &
                                       rowCSRMatrixGetValueReal8 => rowCSRMatrixGetValue, &
                                       rowCSRMatrixGetPointerOnValueReal8 => rowCSRMatrixGetPointerOnValue, &
                                       rowCSRMatrixSetToZeroReal8 => rowCSRMatrixSetToZero, &
                                       rowCSRMatrixSetToValueReal8 => rowCSRMatrixSetToValue, &
                                       rowCSRMatrixInsertValueReal8 => rowCSRMatrixInsertValue, &
                                       rowCSRMatrixAddValueReal8 => rowCSRMatrixAddValue

! Interface
! =========

   INTERFACE rowCSRMatrixGetValue
      MODULE PROCEDURE rowCSRMatrixGetValueReal8, rowCSRMatrixGetValueReal4, &
                       rowCSRMatrixGetValueInteger2, rowCSRMatrixGetValueInteger4, rowCSRMatrixGetValueInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixGetValues
      MODULE PROCEDURE rowCSRMatrixGetValuesReal8, rowCSRMatrixGetValuesReal4, &
                       rowCSRMatrixGetValuesInteger2, rowCSRMatrixGetValuesInteger4, rowCSRMatrixGetValuesInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixGetIndex
      MODULE PROCEDURE rowCSRMatrixGetValuesReal8, rowCSRMatrixGetValuesReal4, &
                       rowCSRMatrixGetValuesInteger2, rowCSRMatrixGetValuesInteger4, rowCSRMatrixGetValuesInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixGetPointerOnValue
      MODULE PROCEDURE rowCSRMatrixGetPointerOnValueReal8, rowCSRMatrixGetPointerOnValueReal4, &
                       rowCSRMatrixGetPointerOnValueInteger2, rowCSRMatrixGetPointerOnValueInteger4, &
                       rowCSRMatrixGetPointerOnValueInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixSetToZero
      MODULE PROCEDURE rowCSRMatrixSetToZeroReal8, rowCSRMatrixSetToZeroReal4, &
                       rowCSRMatrixSetToZeroInteger2, rowCSRMatrixSetToZeroInteger4, rowCSRMatrixSetToZeroInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixSetToValue
      MODULE PROCEDURE rowCSRMatrixSetToValueReal8, rowCSRMatrixSetToValueReal4, &
                       rowCSRMatrixSetToValueInteger2, rowCSRMatrixSetToValueInteger4, rowCSRMatrixSetToValueInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixInsertValue
      MODULE PROCEDURE rowCSRMatrixInsertValueReal8, rowCSRMatrixInsertValueReal4, &
                       rowCSRMatrixInsertValueInteger2, rowCSRMatrixInsertValueInteger4, rowCSRMatrixInsertValueInteger8
   END INTERFACE

   INTERFACE rowCSRMatrixAddValue
      MODULE PROCEDURE rowCSRMatrixAddValueReal8, rowCSRMatrixAddValueReal4, &
                       rowCSRMatrixAddValueInteger2, rowCSRMatrixAddValueInteger4, rowCSRMatrixAddValueInteger8
   END INTERFACE

END MODULE rowCSRMatrixInterface

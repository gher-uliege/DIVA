MODULE moduleLineDataBase

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
   USE moduleGenericTypeDataBaseDefinition
   USE moduleGenericDataBase, ONLY : dataBaseCreateBaseLine => dataBaseCreateBase, &
                                     dataBaseCreateWithDimensionLine => dataBaseCreateWithDimension, &
                                     dataBaseCreateWithDimensionAndFirstIndexLine => dataBaseCreateWithDimensionAndFirstIndex, &
                                     dataBaseSetSizeLine => dataBaseSetSize, &
                                     dataBasePrintInformationLine => dataBasePrintInformation, &
                                     dataBaseInsertElementLine => dataBaseInsertElement, &
                                     dataBaseFastInsertElementLine => dataBaseFastInsertElement, &
                                     dataBaseGetValuesLine => dataBaseGetValues, &
                                     dataBaseGetAllocationStatusLine => dataBaseGetAllocationStatus, &
                                     dataBaseGetPointerOnValueLine => dataBaseGetPointerOnValue, &
                                     dataBaseSetIncreaseSizeLine => dataBaseSetIncreaseSize, &
                                     dataBaseDestroyLine => dataBaseDestroy, &
                                     dataBaseGetFirstIndexLine => dataBaseGetFirstIndex, &
                                     dataBaseGetLastIndexLine => dataBaseGetLastIndex, &
                                     dataBaseGetSizeLine => dataBaseGetSize, &
                                     dataBaseGetAllocatedSizeLine => dataBaseGetAllocatedSize, &
                                     dataBaseGetIncreaseSizeLine => dataBaseGetIncreaseSize, &
                                     dataBaseGetDefaultIncreaseSizeLine => dataBaseGetDefaultIncreaseSize, &
                                     dataBaseInitialiseLine => dataBaseInitialise, &
                                     dataBaseSetValueLine => dataBaseSetValue, &
                                     dataBaseOptimizeMemoryLine => dataBaseOptimizeMemory, &
                                     dataBaseAddSizeLine => dataBaseAddSize, &
                                     dataBasePushBackElementLine => dataBasePushBackElement, &
                                     dataBaseFastPushBackElementLine => dataBaseFastPushBackElement, &
                                     dataBaseGetPointerOnLastValueLine => dataBaseGetPointerOnLastValue

! Rem. : redirection of function pointer xxxLine => xxx needed for intel compiler (not for g95 or gfortran)

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE lineDBCreate
      MODULE PROCEDURE dataBaseCreateBaseLine, dataBaseCreateWithDimensionLine, &
                       dataBaseCreateWithDimensionAndFirstIndexLine
   END INTERFACE

   INTERFACE lineDBSetSize
      MODULE PROCEDURE dataBaseSetSizeLine
   END INTERFACE

   INTERFACE lineDBPrint
      MODULE PROCEDURE dataBasePrintInformationLine
   END INTERFACE

   INTERFACE lineDBInsert
      MODULE PROCEDURE dataBaseInsertElementLine
   END INTERFACE

   INTERFACE lineDBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElementLine
   END INTERFACE

   INTERFACE lineDBGetValues
      MODULE PROCEDURE dataBaseGetValuesLine
   END INTERFACE

   INTERFACE lineDBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatusLine
   END INTERFACE

   INTERFACE lineDBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValueLine
   END INTERFACE

   INTERFACE lineDBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSizeLine
   END INTERFACE

   INTERFACE lineDBDestroy
      MODULE PROCEDURE dataBaseDestroyLine
   END INTERFACE

   INTERFACE lineDBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndexLine
   END INTERFACE

   INTERFACE lineDBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndexLine
   END INTERFACE

   INTERFACE lineDBGetSize
      MODULE PROCEDURE dataBaseGetSizeLine
   END INTERFACE

   INTERFACE lineDBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSizeLine
   END INTERFACE

   INTERFACE lineDBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSizeLine
   END INTERFACE

   INTERFACE lineDBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSizeLine
   END INTERFACE

   INTERFACE lineDBInitialise
      MODULE PROCEDURE dataBaseInitialiseLine
   END INTERFACE

   INTERFACE lineDBSetValue
      MODULE PROCEDURE dataBaseSetValueLine
   END INTERFACE

   INTERFACE lineDBOptimizeMemory
      MODULE PROCEDURE dataBaseOptimizeMemoryLine
   END INTERFACE

   INTERFACE lineDBAddSize
      MODULE PROCEDURE dataBaseAddSizeLine
   END INTERFACE

   INTERFACE lineDBPushBack
      MODULE PROCEDURE dataBasePushBackElementLine
   END INTERFACE

   INTERFACE lineDBFastPushBack
      MODULE PROCEDURE dataBaseFastPushBackElementLine
   END INTERFACE

   INTERFACE lineDBGetPointerOnLastValue
      MODULE PROCEDURE dataBaseGetPointerOnLastValueLine
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
 !CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

END MODULE moduleLineDataBase

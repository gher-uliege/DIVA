MODULE moduleTria3DataBase

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
   USE moduleGenericDataBase, ONLY : dataBaseCreateBaseTria3 => dataBaseCreateBase, &
                                     dataBaseCreateWithDimensionTria3 => dataBaseCreateWithDimension, &
                                     dataBaseCreateWithDimensionAndFirstIndexTria3 => dataBaseCreateWithDimensionAndFirstIndex, &
                                     dataBaseSetSizeTria3 => dataBaseSetSize, &
                                     dataBasePrintInformationTria3 => dataBasePrintInformation, &
                                     dataBaseInsertElementTria3 => dataBaseInsertElement, &
                                     dataBaseFastInsertElementTria3 => dataBaseFastInsertElement, &
                                     dataBaseGetValuesTria3 => dataBaseGetValues, &
                                     dataBaseGetAllocationStatusTria3 => dataBaseGetAllocationStatus, &
                                     dataBaseGetPointerOnValueTria3 => dataBaseGetPointerOnValue, &
                                     dataBaseSetIncreaseSizeTria3 => dataBaseSetIncreaseSize, &
                                     dataBaseDestroyTria3 => dataBaseDestroy, &
                                     dataBaseGetFirstIndexTria3 => dataBaseGetFirstIndex, &
                                     dataBaseGetLastIndexTria3 => dataBaseGetLastIndex, &
                                     dataBaseGetSizeTria3 => dataBaseGetSize, &
                                     dataBaseGetAllocatedSizeTria3 => dataBaseGetAllocatedSize, &
                                     dataBaseGetIncreaseSizeTria3 => dataBaseGetIncreaseSize, &
                                     dataBaseGetDefaultIncreaseSizeTria3 => dataBaseGetDefaultIncreaseSize, &
                                     dataBaseInitialiseTria3 => dataBaseInitialise, &
                                     dataBaseSetValueTria3 => dataBaseSetValue, &
                                     dataBaseOptimizeMemoryTria3 => dataBaseOptimizeMemory, &
                                     dataBaseAddSizeTria3 => dataBaseAddSize, &
                                     dataBasePushBackElementTria3 => dataBasePushBackElement, &
                                     dataBaseFastPushBackElementTria3 => dataBaseFastPushBackElement, &
                                     dataBaseGetPointerOnLastValueTria3 => dataBaseGetPointerOnLastValue

! Rem. : redirection of function pointer xxxTria3 => xxx needed for intel compiler (not for g95 or gfortran)

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE tria3DBCreate
      MODULE PROCEDURE dataBaseCreateBaseTria3, dataBaseCreateWithDimensionTria3, &
                       dataBaseCreateWithDimensionAndFirstIndexTria3
   END INTERFACE

   INTERFACE tria3DBSetSize
      MODULE PROCEDURE dataBaseSetSizeTria3
   END INTERFACE

   INTERFACE tria3DBPrint
      MODULE PROCEDURE dataBasePrintInformationTria3
   END INTERFACE

   INTERFACE tria3DBInsert
      MODULE PROCEDURE dataBaseInsertElementTria3
   END INTERFACE

   INTERFACE tria3DBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElementTria3
   END INTERFACE

   INTERFACE tria3DBGetValues
      MODULE PROCEDURE dataBaseGetValuesTria3
   END INTERFACE

   INTERFACE tria3DBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatusTria3
   END INTERFACE

   INTERFACE tria3DBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValueTria3
   END INTERFACE

   INTERFACE tria3DBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSizeTria3
   END INTERFACE

   INTERFACE tria3DBDestroy
      MODULE PROCEDURE dataBaseDestroyTria3
   END INTERFACE

   INTERFACE tria3DBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndexTria3
   END INTERFACE

   INTERFACE tria3DBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndexTria3
   END INTERFACE

   INTERFACE tria3DBGetSize
      MODULE PROCEDURE dataBaseGetSizeTria3
   END INTERFACE

   INTERFACE tria3DBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSizeTria3
   END INTERFACE

   INTERFACE tria3DBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSizeTria3
   END INTERFACE

   INTERFACE tria3DBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSizeTria3
   END INTERFACE

   INTERFACE tria3DBInitialise
      MODULE PROCEDURE dataBaseInitialiseTria3
   END INTERFACE

   INTERFACE tria3DBSetValue
      MODULE PROCEDURE dataBaseSetValueTria3
   END INTERFACE

   INTERFACE tria3DBOptimizeMemory
      MODULE PROCEDURE dataBaseOptimizeMemoryTria3
   END INTERFACE

   INTERFACE tria3DBAddSize
      MODULE PROCEDURE dataBaseAddSizeTria3
   END INTERFACE

   INTERFACE tria3DBPushBack
      MODULE PROCEDURE dataBasePushBackElementTria3
   END INTERFACE

   INTERFACE tria3DBFastPushBack
      MODULE PROCEDURE dataBaseFastPushBackElementTria3
   END INTERFACE

   INTERFACE tria3DBGetPointerOnLastValue
      MODULE PROCEDURE dataBaseGetPointerOnLastValueTria3
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

END MODULE moduleTria3DataBase

MODULE moduleStencilDataBase

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
   USE moduleGenericDataBase, ONLY : dataBaseCreateBaseStencil => dataBaseCreateBase, &
                                     dataBaseCreateWithDimensionStencil => dataBaseCreateWithDimension, &
                                     dataBaseCreateWithDimensionAndFirstIndexStencil => dataBaseCreateWithDimensionAndFirstIndex, &
                                     dataBaseSetSizeStencil => dataBaseSetSize, &
                                     dataBasePrintInformationStencil => dataBasePrintInformation, &
                                     dataBaseInsertElementStencil => dataBaseInsertElement, &
                                     dataBaseFastInsertElementStencil => dataBaseFastInsertElement, &
                                     dataBaseGetValuesStencil => dataBaseGetValues, &
                                     dataBaseGetAllocationStatusStencil => dataBaseGetAllocationStatus, &
                                     dataBaseGetPointerOnValueStencil => dataBaseGetPointerOnValue, &
                                     dataBaseSetIncreaseSizeStencil => dataBaseSetIncreaseSize, &
                                     dataBaseDestructorStencil => dataBaseDestructor, &
                                     dataBaseGetFirstIndexStencil => dataBaseGetFirstIndex, &
                                     dataBaseGetLastIndexStencil => dataBaseGetLastIndex, &
                                     dataBaseGetSizeStencil => dataBaseGetSize, &
                                     dataBaseGetAllocatedSizeStencil => dataBaseGetAllocatedSize, &
                                     dataBaseGetIncreaseSizeStencil => dataBaseGetIncreaseSize, &
                                     dataBaseGetDefaultIncreaseSizeStencil => dataBaseGetDefaultIncreaseSize, &
                                     dataBaseInitialiseStencil => dataBaseInitialise, &
                                     dataBaseSetValueStencil => dataBaseSetValue, &
                                     dataBaseOptimizeMemoryStencil => dataBaseOptimizeMemory

! Rem. : redirection of function pointer xxxLine => xxx needed for intel compiler (not for g95 or gfortran)

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE stencilDBCreate
      MODULE PROCEDURE dataBaseCreateBaseStencil, dataBaseCreateWithDimensionStencil, &
                       dataBaseCreateWithDimensionAndFirstIndexStencil
   END INTERFACE

   INTERFACE stencilDBSetSize
      MODULE PROCEDURE dataBaseSetSizeStencil
   END INTERFACE

   INTERFACE stencilDBPrint
      MODULE PROCEDURE dataBasePrintInformationStencil
   END INTERFACE

   INTERFACE stencilDBInsert
      MODULE PROCEDURE dataBaseInsertElementStencil
   END INTERFACE

   INTERFACE stencilDBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElementStencil
   END INTERFACE

   INTERFACE stencilDBGetValues
      MODULE PROCEDURE dataBaseGetValuesStencil
   END INTERFACE

   INTERFACE stencilDBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatusStencil
   END INTERFACE

   INTERFACE stencilDBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValueStencil
   END INTERFACE

   INTERFACE stencilDBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSizeStencil
   END INTERFACE

   INTERFACE stencilDBDestroy
      MODULE PROCEDURE dataBaseDestructorStencil
   END INTERFACE

   INTERFACE stencilDBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndexStencil
   END INTERFACE

   INTERFACE stencilDBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndexStencil
   END INTERFACE

   INTERFACE stencilDBGetSize
      MODULE PROCEDURE dataBaseGetSizeStencil
   END INTERFACE

   INTERFACE stencilDBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSizeStencil
   END INTERFACE

   INTERFACE stencilDBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSizeStencil
   END INTERFACE

   INTERFACE stencilDBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSizeStencil
   END INTERFACE

   INTERFACE stencilDBInitialise
      MODULE PROCEDURE dataBaseInitialiseStencil
   END INTERFACE

   INTERFACE stencilDBSetValue
      MODULE PROCEDURE dataBaseSetValueStencil
   END INTERFACE

   INTERFACE stencilDBOptimizeMemory
      MODULE PROCEDURE dataBaseOptimizeMemoryStencil
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

END MODULE moduleStencilDataBase

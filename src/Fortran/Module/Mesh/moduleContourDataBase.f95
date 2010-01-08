MODULE moduleContourDataBase

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
   USE moduleGenericDataBase, ONLY : dataBaseCreateBaseContour => dataBaseCreateBase, &
                                     dataBaseCreateWithDimensionContour => dataBaseCreateWithDimension, &
                                     dataBaseCreateWithDimensionAndFirstIndexContour => dataBaseCreateWithDimensionAndFirstIndex, &
                                     dataBaseSetSizeContour => dataBaseSetSize, &
                                     dataBasePrintInformationContour => dataBasePrintInformation, &
                                     dataBaseInsertElementContour => dataBaseInsertElement, &
                                     dataBaseFastInsertElementContour => dataBaseFastInsertElement, &
                                     dataBaseGetValuesContour => dataBaseGetValues, &
                                     dataBaseGetAllocationStatusContour => dataBaseGetAllocationStatus, &
                                     dataBaseGetPointerOnValueContour => dataBaseGetPointerOnValue, &
                                     dataBaseSetIncreaseSizeContour => dataBaseSetIncreaseSize, &
                                     dataBaseDestructorContour => dataBaseDestructor, &
                                     dataBaseGetFirstIndexContour => dataBaseGetFirstIndex, &
                                     dataBaseGetLastIndexContour => dataBaseGetLastIndex, &
                                     dataBaseGetSizeContour => dataBaseGetSize, &
                                     dataBaseGetAllocatedSizeContour => dataBaseGetAllocatedSize, &
                                     dataBaseGetIncreaseSizeContour => dataBaseGetIncreaseSize, &
                                     dataBaseGetDefaultIncreaseSizeContour => dataBaseGetDefaultIncreaseSize, &
                                     dataBaseInitialiseContour => dataBaseInitialise, &
                                     dataBaseSetValueContour => dataBaseSetValue, &
                                     dataBaseOptimizeMemoryContour => dataBaseOptimizeMemory, &
                                     dataBaseAddSizeContour => dataBaseAddSize, &
                                     dataBasePushBackElementContour => dataBasePushBackElement, &
                                     dataBaseFastPushBackElementContour => dataBaseFastPushBackElement, &
                                     dataBaseGetPointerOnLastValueContour => dataBaseGetPointerOnLastValue

! Rem. : redirection of function pointer xxxContour => xxx needed for intel compiler (not for g95 or gfortran)

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE contourDBCreate
      MODULE PROCEDURE dataBaseCreateBaseContour, dataBaseCreateWithDimensionContour, &
                       dataBaseCreateWithDimensionAndFirstIndexContour
   END INTERFACE

   INTERFACE contourDBSetSize
      MODULE PROCEDURE dataBaseSetSizeContour
   END INTERFACE

   INTERFACE contourDBPrint
      MODULE PROCEDURE dataBasePrintInformationContour
   END INTERFACE

   INTERFACE contourDBInsert
      MODULE PROCEDURE dataBaseInsertElementContour
   END INTERFACE

   INTERFACE contourDBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElementContour
   END INTERFACE

   INTERFACE contourDBGetValues
      MODULE PROCEDURE dataBaseGetValuesContour
   END INTERFACE

   INTERFACE contourDBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatusContour
   END INTERFACE

   INTERFACE contourDBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValueContour
   END INTERFACE

   INTERFACE contourDBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSizeContour
   END INTERFACE

   INTERFACE contourDBDestroy
      MODULE PROCEDURE dataBaseDestructorContour
   END INTERFACE

   INTERFACE contourDBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndexContour
   END INTERFACE

   INTERFACE contourDBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndexContour
   END INTERFACE

   INTERFACE contourDBGetSize
      MODULE PROCEDURE dataBaseGetSizeContour
   END INTERFACE

   INTERFACE contourDBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSizeContour
   END INTERFACE

   INTERFACE contourDBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSizeContour
   END INTERFACE

   INTERFACE contourDBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSizeContour
   END INTERFACE

   INTERFACE contourDBInitialise
      MODULE PROCEDURE dataBaseInitialiseContour
   END INTERFACE

   INTERFACE contourDBSetValue
      MODULE PROCEDURE dataBaseSetValueContour
   END INTERFACE

   INTERFACE contourDBOptimizeMemory
      MODULE PROCEDURE dataBaseOptimizeMemoryContour
   END INTERFACE

   INTERFACE contourDBAddSize
      MODULE PROCEDURE dataBaseAddSizeContour
   END INTERFACE

   INTERFACE contourDBPushBack
      MODULE PROCEDURE dataBasePushBackElementContour
   END INTERFACE

   INTERFACE contourDBFastPushBack
      MODULE PROCEDURE dataBaseFastPushBackElementContour
   END INTERFACE

   INTERFACE contourDBGetPointerOnLastValue
      MODULE PROCEDURE dataBaseGetPointerOnLastValueContour
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

END MODULE moduleContourDataBase

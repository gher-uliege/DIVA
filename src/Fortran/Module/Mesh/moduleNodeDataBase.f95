MODULE moduleNodeDataBase

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
   USE moduleGenericDataBase, ONLY : dataBaseCreateBaseNode => dataBaseCreateBase, &
                                     dataBaseCreateWithDimensionNode => dataBaseCreateWithDimension, &
                                     dataBaseCreateWithDimensionAndFirstIndexNode => dataBaseCreateWithDimensionAndFirstIndex, &
                                     dataBaseSetSizeNode => dataBaseSetSize, &
                                     dataBasePrintInformationNode => dataBasePrintInformation, &
                                     dataBaseInsertElementNode => dataBaseInsertElement, &
                                     dataBaseFastInsertElementNode => dataBaseFastInsertElement, &
                                     dataBaseGetValuesNode => dataBaseGetValues, &
                                     dataBaseGetAllocationStatusNode => dataBaseGetAllocationStatus, &
                                     dataBaseGetPointerOnValueNode => dataBaseGetPointerOnValue, &
                                     dataBaseSetIncreaseSizeNode => dataBaseSetIncreaseSize, &
                                     dataBaseDestructorNode => dataBaseDestructor, &
                                     dataBaseGetFirstIndexNode => dataBaseGetFirstIndex, &
                                     dataBaseGetLastIndexNode => dataBaseGetLastIndex, &
                                     dataBaseGetSizeNode => dataBaseGetSize, &
                                     dataBaseGetAllocatedSizeNode => dataBaseGetAllocatedSize, &
                                     dataBaseGetIncreaseSizeNode => dataBaseGetIncreaseSize, &
                                     dataBaseGetDefaultIncreaseSizeNode => dataBaseGetDefaultIncreaseSize, &
                                     dataBaseInitialiseNode => dataBaseInitialise, &
                                     dataBaseSetValueNode => dataBaseSetValue, &
                                     dataBaseOptimizeMemoryNode => dataBaseOptimizeMemory

! Rem. : redirection of function pointer xxxLine => xxx needed for intel compiler (not for g95 or gfortran)

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE nodeDBCreate
      MODULE PROCEDURE dataBaseCreateBaseNode, dataBaseCreateWithDimensionNode, &
                       dataBaseCreateWithDimensionAndFirstIndexNode
   END INTERFACE

   INTERFACE nodeDBSetSize
      MODULE PROCEDURE dataBaseSetSizeNode
   END INTERFACE

   INTERFACE nodeDBPrint
      MODULE PROCEDURE dataBasePrintInformationNode
   END INTERFACE

   INTERFACE nodeDBInsert
      MODULE PROCEDURE dataBaseInsertElementNode
   END INTERFACE

   INTERFACE nodeDBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElementNode
   END INTERFACE

   INTERFACE nodeDBGetValues
      MODULE PROCEDURE dataBaseGetValuesNode
   END INTERFACE

   INTERFACE nodeDBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatusNode
   END INTERFACE

   INTERFACE nodeDBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValueNode
   END INTERFACE

   INTERFACE nodeDBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSizeNode
   END INTERFACE

   INTERFACE nodeDBDestroy
      MODULE PROCEDURE dataBaseDestructorNode
   END INTERFACE

   INTERFACE nodeDBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndexNode
   END INTERFACE

   INTERFACE nodeDBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndexNode
   END INTERFACE

   INTERFACE nodeDBGetSize
      MODULE PROCEDURE dataBaseGetSizeNode
   END INTERFACE

   INTERFACE nodeDBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSizeNode
   END INTERFACE

   INTERFACE nodeDBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSizeNode
   END INTERFACE

   INTERFACE nodeDBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSizeNode
   END INTERFACE

   INTERFACE nodeDBInitialise
      MODULE PROCEDURE dataBaseInitialiseNode
   END INTERFACE

   INTERFACE nodeDBSetValue
      MODULE PROCEDURE dataBaseSetValueNode
   END INTERFACE

   INTERFACE nodeDBOptimizeMemory
      MODULE PROCEDURE dataBaseOptimizeMemoryNode
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

END MODULE moduleNodeDataBase

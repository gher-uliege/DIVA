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
   USE moduleGenericDataBase

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------

! Interface
! =========
   INTERFACE lineDBCreate
      MODULE PROCEDURE dataBaseCreateBase, dataBaseCreateWithDimension, dataBaseCreateWithDimensionAndFirstIndex
   END INTERFACE

   INTERFACE lineDBSetSize
      MODULE PROCEDURE dataBaseSetSize
   END INTERFACE

   INTERFACE lineDBPrint
      MODULE PROCEDURE dataBasePrintInformation
   END INTERFACE

   INTERFACE lineDBInsert
      MODULE PROCEDURE dataBaseInsertElement
   END INTERFACE

   INTERFACE lineDBFastInsert
      MODULE PROCEDURE dataBaseFastInsertElement
   END INTERFACE

   INTERFACE lineDBGetValues
      MODULE PROCEDURE dataBaseGetValues
   END INTERFACE

   INTERFACE lineDBGetAllocationStatus
      MODULE PROCEDURE dataBaseGetAllocationStatus
   END INTERFACE

   INTERFACE lineDBGetValue
      MODULE PROCEDURE dataBaseGetPointerOnValue
   END INTERFACE

   INTERFACE lineDBSetIncreaseSize
      MODULE PROCEDURE dataBaseSetIncreaseSize
   END INTERFACE

   INTERFACE lineDBDestroy
      MODULE PROCEDURE dataBaseDestructor
   END INTERFACE

   INTERFACE lineDBGetFirstIndex
      MODULE PROCEDURE dataBaseGetFirstIndex
   END INTERFACE

   INTERFACE lineDBGetLastIndex
      MODULE PROCEDURE dataBaseGetLastIndex
   END INTERFACE

   INTERFACE lineDBGetSize
      MODULE PROCEDURE dataBaseGetSize
   END INTERFACE

   INTERFACE lineDBGetAllocatedSize
      MODULE PROCEDURE dataBaseGetAllocatedSize
   END INTERFACE

   INTERFACE lineDBGetIncreaseSize
      MODULE PROCEDURE dataBaseGetIncreaseSize
   END INTERFACE

   INTERFACE lineDBGetDefaultIncreaseSize
      MODULE PROCEDURE dataBaseGetDefaultIncreaseSize
   END INTERFACE

   INTERFACE lineDBInitialise
      MODULE PROCEDURE dataBaseInitialise
   END INTERFACE

   INTERFACE lineDBSetValue
      MODULE PROCEDURE dataBaseSetValue
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

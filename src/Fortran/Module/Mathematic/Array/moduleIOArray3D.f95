MODULE moduleIOArray3D

#ifdef _REAL_

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
   USE moduleFileDefinition
   USE moduleRead, ONLY : readArray
   USE moduleWorkingArray, ONLY : workingArray
   USE moduleMemoryArrayManagement, ONLY : memoryDestructor, memorySetAllocatedSize,  &
                                           memorySetFirstIndex, memorySetSize, memoryDefineLastIndex, &
                                           memorySetAllocationStatus

   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: ioArrayRead


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
 CONTAINS


! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================
! Procedure 1 : read data from disk
! --------------------------------
! ----------------------------------
  SUBROUTINE ioArrayRead(fileToRead,exclusionValue)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToRead
      REAL(KIND=4), INTENT(OUT) :: exclusionValue
      INTEGER :: nbOfDataI,nbOfDataJ,nbOfDataK

!     Body
!     - - -

      CALL memoryDestructor()
      CALL readArray(fileToRead,workingArray%values,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
      CALL memorySetFirstIndex(ione,ione,ione)
      CALL memorySetAllocatedSize(nbOfDataI,nbOfDataJ,nbOfDataK)
      CALL memorySetSize(nbOfDataI,nbOfDataJ,nbOfDataK)
      CALL memoryDefineLastIndex()
      CALL memorySetAllocationStatus(true)

  END SUBROUTINE

#endif

END MODULE moduleIOArray3D

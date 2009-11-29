MODULE moduleIOArray2D

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
   USE moduleRead, ONLY : readMatrix
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
      INTEGER :: nbOfDataI, nbOfDataJ

!     Body
!     - - -

      CALL memoryDestructor()
      CALL readMatrix(fileToRead,workingArray%values,exclusionValue,nbOfDataI,nbOfDataJ)
      CALL memorySetFirstIndex(ione,ione)
      CALL memorySetAllocatedSize(nbOfDataI,nbOfDataJ)
      CALL memorySetSize(nbOfDataI,nbOfDataJ)
      CALL memoryDefineLastIndex()
      CALL memorySetAllocationStatus(true)

  END SUBROUTINE

#endif

END MODULE moduleIOArray2D

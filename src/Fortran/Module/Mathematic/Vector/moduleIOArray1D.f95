MODULE moduleIOArray1D

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
   USE moduleRead, ONLY : readVectorGHER
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
      INTEGER :: nbOfDataI

!     Body
!     - - -

      CALL memoryDestructor()
      CALL readVectorGHER(fileToRead,workingArray%values,exclusionValue,nbOfDataI)
      CALL memorySetFirstIndex(ione)
      CALL memorySetAllocatedSize(nbOfDataI)
      CALL memorySetSize(nbOfDataI)
      CALL memoryDefineLastIndex()
      CALL memorySetAllocationStatus(true)

  END SUBROUTINE

#endif

END MODULE moduleIOArray1D

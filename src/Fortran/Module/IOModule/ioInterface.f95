MODULE ioInterface
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
   USE logicalUnitManager
   USE moduleFile
   USE moduleIOBase, ONLY : setFileFormType
   USE moduleReadReal4, ONLY : readVectorReal4 => readVector, &
                               readMatrixReal4 => readMatrix, &
                               readArrayReal4 => readArray
   USE moduleReadReal8, ONLY : readVectorReal8 => readVector, &
                               readMatrixReal8 => readMatrix, &
                               readArrayReal8 => readArray
   USE moduleWriteReal4, ONLY : writeVectorReal4 => writeVector, &
                                writeMatrixReal4 => writeMatrix, &
                                writeArrayReal4 => writeArray, &
                                writeDataReal4 => writeData
   USE moduleWriteReal8, ONLY : writeVectorReal8 => writeVector, &
                                writeMatrixReal8 => writeMatrix, &
                                writeArrayReal8=> writeArray, &
                                writeDataReal8 => writeData

   INCLUDE 'constantParameter.h'

! Interface
! =========
   INTERFACE readFromDisk
      MODULE PROCEDURE readVectorReal4, readVectorReal8, &
                       readMatrixReal4, readMatrixReal8, &
                       readArrayReal4, readArrayReal8
   END INTERFACE

   INTERFACE writeOnDisk
      MODULE PROCEDURE writeVectorReal4, writeVectorReal8, &
                       writeMatrixReal4, writeMatrixReal8, &
                       writeArrayReal4, writeArrayReal8
   END INTERFACE

! Procedure status
! ================
  PUBLIC :: uwritc  ! for compatibility with previous version

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

! Procedure 1 : writeData to file
! -------------------------------
  SUBROUTINE uwritc(fileUnit,entries8,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: fileUnit, iprecision, nbOfDataI, nbOfDataJ, nbOfDataK
      INTEGER, INTENT(INOUT) :: nbOfWords
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      REAL(KIND=8), INTENT(IN) :: entries8(*)
      REAL(KIND=4), INTENT(IN) :: entries4(*)

!     Body
!     - - -
      CALL setFileFormType(GHER)
      IF ( iprecision == ifour ) THEN
         CALL writeDataReal4(fileUnit,entries4,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      ELSEIF ( iprecision == ieight ) THEN
         CALL writeDataReal8(fileUnit,entries8,exclusionValue,iprecision,nbOfDataI, nbOfDataJ, nbOfDataK, nbOfWords)
      END IF

  END SUBROUTINE

END MODULE ioInterface

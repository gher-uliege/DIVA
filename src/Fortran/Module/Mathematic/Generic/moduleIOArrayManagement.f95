MODULE moduleIOArrayManagement

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

! Preprocessing declaration
! =========================
#define _ARRAY_1D_DEFINITION_
#define _ARRAY_2D_DEFINITION_
#define _ARRAY_3D_DEFINITION_

#ifdef _ARRAY_1D_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_
#endif

#ifdef _ARRAY_2D_
#undef _ARRAY_3D_DEFINITION_
#endif

! Include file
! ============
   USE moduleFileDefinition
#ifdef _ARRAY_1D_
   USE moduleWrite, ONLY : writeVector
#endif
#ifdef _ARRAY_2D_
   USE moduleWrite, ONLY : writeMatrix
#endif
#ifdef _ARRAY_3D_
   USE moduleWrite, ONLY : writeArray
#endif

   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, memoryGetIncreaseSizeX, &
#ifdef _ARRAY_2D_DEFINITION_
                                           memoryGetFirstIndexY, memoryGetLastIndexY , memoryGetSizeY ,&
                                           memoryGetIncreaseSizeY, &
#endif
#ifdef _ARRAY_3D_DEFINITION_
                                           memoryGetFirstIndexZ, memoryGetLastIndexZ , memoryGetSizeZ ,&
                                           memoryGetIncreaseSizeZ, &
#endif
                                           memoryGetLastIndexX, memoryGetSizeX

   USE moduleValuesArrayManagement, ONLY : memoryGetValues

   INCLUDE 'constantParameter.h'
   INCLUDE 'logicalParameter.h'

! Declaration
! ===========

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: ioArrayWrite


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
! Procedure 1 : write data on disk
! --------------------------------
! ----------------------------------
  SUBROUTINE ioArrayWrite(fileToWrite,exclusionValue)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      INTEGER :: istartX, iendX, lengthX, increaseSizeX
#ifdef _ARRAY_2D_DEFINITION_
      INTEGER :: istartY, iendY, lengthY, increaseSizeY
#endif
#ifdef _ARRAY_3D_DEFINITION_
      INTEGER :: istartZ, iendZ, lengthZ, increaseSizeZ
#endif

#ifdef _ARRAY_1D_
      VARType, DIMENSION(:), POINTER :: ptr
#endif
#ifdef _ARRAY_2D_
      VARType, DIMENSION(:,:), POINTER :: ptr
#endif
#ifdef _ARRAY_3D_
      VARType, DIMENSION(:,:,:), POINTER :: ptr
#endif


!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      increaseSizeX = memoryGetIncreaseSizeX()
      lengthX = memoryGetSizeX()

#ifdef _ARRAY_2D_DEFINITION_
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      increaseSizeY = memoryGetIncreaseSizeY()
      lengthY = memoryGetSizeY()
#endif

#ifdef _ARRAY_3D_DEFINITION_
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()
      increaseSizeZ = memoryGetIncreaseSizeZ()
      lengthZ = memoryGetSizeZ()
#endif

      ptr =>  memoryGetValues()

#ifdef _ARRAY_1D_
      CALL  writeVector(fileToWrite,ptr(istartX:iendX),increaseSizeX,lengthX,istartX,iendX,exclusionValue,false)
#endif
#ifdef _ARRAY_2D_
      CALL  writeMatrix(fileToWrite,ptr(istartX:iendX,istartY:iendY),increaseSizeX,lengthX,istartX,iendX, &
                                               increaseSizeY,lengthY,istartY,iendY,exclusionValue,false)
#endif
#ifdef _ARRAY_3D_
      CALL  writeArray(fileToWrite,ptr(istartX:iendX,istartY:iendY,istartZ:iendZ),increaseSizeX,lengthX,istartX,iendX, &
                                               increaseSizeY,lengthY,istartY,iendY, &
                                               increaseSizeZ,lengthZ,istartZ,iendZ,exclusionValue,false)
#endif

  END SUBROUTINE

#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

END MODULE moduleIOArrayManagement

MODULE moduleIOArrayManagement

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

   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, &
#ifdef _ARRAY_2D_DEFINITION_
                                           memoryGetFirstIndexY, memoryGetLastIndexY , memoryGetSizeY ,&
#endif
#ifdef _ARRAY_3D_DEFINITION_
                                           memoryGetFirstIndexZ, memoryGetLastIndexZ , memoryGetSizeZ ,&
#endif
                                           memoryGetLastIndexX, memoryGetSizeX

   USE moduleValuesArrayManagement, ONLY : memoryGetValues

   INCLUDE 'constantParameter.h'

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
      INTEGER :: istartX, iendX, lengthX
#ifdef _ARRAY_2D_DEFINITION_
      INTEGER :: istartY, iendY, lengthY
#endif
#ifdef _ARRAY_3D_DEFINITION_
      INTEGER :: istartZ, iendZ, lengthZ
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
      lengthX = memoryGetSizeX()

#ifdef _ARRAY_2D_DEFINITION_
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      lengthY = memoryGetSizeY()
#endif

#ifdef _ARRAY_3D_DEFINITION_
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()
      lengthZ = memoryGetSizeZ()
#endif

      ptr =>  memoryGetValues()

#ifdef _ARRAY_1D_
      CALL  writeVector(fileToWrite,ptr(istartX:iendX),exclusionValue,lengthX)
#endif
#ifdef _ARRAY_2D_
      CALL  writeMatrix(fileToWrite,ptr(istartX:iendX,istartY:iendY),exclusionValue,lengthX,lengthY)
#endif
#ifdef _ARRAY_3D_
      CALL  writeArray(fileToWrite,ptr(istartX:iendX,istartY:iendY,istartZ:iendZ),exclusionValue,lengthX,lengthY,lengthZ)
#endif

  END SUBROUTINE

#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

#endif

END MODULE moduleIOArrayManagement

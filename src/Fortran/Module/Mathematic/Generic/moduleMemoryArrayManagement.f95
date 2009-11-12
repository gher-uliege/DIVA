MODULE moduleMemoryArrayManagement

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
   USE moduleWorkingArray, ONLY : workingArray

   INCLUDE 'logicalParameter.h'
   INCLUDE 'constantParameter.h'


! Declaration
! ===========

!  General part
!  ------------
#ifdef _ARRAY_1D_DEFINITION_
   INTEGER, PRIVATE, PARAMETER :: defaultFirstIndexX = 1
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeX = 5
#endif
#ifdef _ARRAY_2D_DEFINITION_
   INTEGER, PRIVATE, PARAMETER :: defaultFirstIndexY = 1
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeY = 5
#endif
#ifdef _ARRAY_3D_DEFINITION_
   INTEGER, PRIVATE, PARAMETER :: defaultFirstIndexZ = 1
   INTEGER, PRIVATE, PARAMETER :: defaultIncreaseSizeZ = 5
#endif


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryGetAllocationStatus
   PUBLIC :: memorySetAllocationStatus
   PUBLIC :: memorySetFirstIndex, memorySetLastIndex, memorySetSize, memorySetAllocatedSize , memorySetIncreaseSize
   PUBLIC :: memoryAllocateMemory, memoryFirstAllocateMemory, memoryDestructor, memoryArrayDefine
   PUBLIC :: memoryDefineLastIndex
   PRIVATE :: computeLastIndex

#ifdef _ARRAY_1D_
   PUBLIC :: memoryGetFirstIndex, memoryGetLastIndex, memoryGetSize, memoryGetAllocatedSize , memoryGetIncreaseSize
   PUBLIC :: memoryGetDefaultIncreaseSize
#endif

#ifdef _ARRAY_1D_DEFINITION_
   PUBLIC :: memoryGetFirstIndexX, memoryGetLastIndexX, memoryGetSizeX, memoryGetAllocatedSizeX , memoryGetIncreaseSizeX
   PUBLIC :: memoryGetDefaultIncreaseSizeX
   PUBLIC :: memorySetFirstIndexX, memorySetLastIndexX, memorySetSizeX, memorySetAllocatedSizeX , memorySetIncreaseSizeX
#endif

#ifdef _ARRAY_2D_DEFINITION_
   PUBLIC :: memoryGetFirstIndexY, memoryGetLastIndexY, memoryGetSizeY, memoryGetAllocatedSizeY , memoryGetIncreaseSizeY
   PUBLIC :: memoryGetDefaultIncreaseSizeY
   PUBLIC :: memorySetFirstIndexY, memorySetLastIndexY, memorySetSizeY, memorySetAllocatedSizeY , memorySetIncreaseSizeY
#endif

#ifdef _ARRAY_3D_DEFINITION_
   PUBLIC :: memoryGetFirstIndexZ, memoryGetLastIndexZ, memoryGetSizeZ, memoryGetAllocatedSizeZ , memoryGetIncreaseSizeZ
   PUBLIC :: memoryGetDefaultIncreaseSizeZ
   PUBLIC :: memorySetFirstIndexZ, memorySetLastIndexZ, memorySetSizeZ, memorySetAllocatedSizeZ , memorySetIncreaseSizeZ
#endif


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
! ===            Internal procedure ("PUBLIC")  : Getting   ===
! =============================================================


! Procedure 1 : get the first index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetFirstIndex() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetFirstIndexX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetFirstIndexX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%firstIndexX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetFirstIndexY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%firstIndexY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetFirstIndexZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%firstIndexZ

  END FUNCTION
#endif

! Procedure 2 : get the last index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetLastIndex() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetLastIndexX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetLastIndexX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%lastIndexX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetLastIndexY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%lastIndexY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetLastIndexZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%lastIndexZ

  END FUNCTION
#endif

! Procedure 3 : get the number of data of the array
! --------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetSizeX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%nbOfDataX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%nbOfDataY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%nbOfDataZ

  END FUNCTION
#endif

! Procedure 4 : get the allocated dimension of the array
! -------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetAllocatedSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetAllocatedSizeX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetAllocatedSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%allocatedSizeX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetAllocatedSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%allocatedSizeY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetAllocatedSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%allocatedSizeZ

  END FUNCTION
#endif

! Procedure 5 : get the increase dimension of the array
! -------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetIncreaseSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetIncreaseSizeX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetIncreaseSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%increaseSizeX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetIncreaseSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%increaseSizeY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetIncreaseSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = workingArray%increaseSizeZ

  END FUNCTION
#endif

! Procedure 6 : get the allocation status
! ----------------------------------------
  FUNCTION memoryGetAllocationStatus() RESULT(status)

!     Declaration
!     - - - - - -
      LOGICAL :: status

!     Body
!     - - -
      status = workingArray%isAllocated

  END FUNCTION


! Procedure 7 : get the default increase size of the array
! -------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetDefaultIncreaseSize() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = memoryGetDefaultIncreaseSizeX()

  END FUNCTION
#endif

#ifdef _ARRAY_1D_DEFINITION_
  FUNCTION memoryGetDefaultIncreaseSizeX() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = defaultIncreaseSizeX

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_DEFINITION_
  FUNCTION memoryGetDefaultIncreaseSizeY() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = defaultIncreaseSizeY

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_DEFINITION_
  FUNCTION memoryGetDefaultIncreaseSizeZ() RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue

!     Body
!     - - -
      ivalue = defaultIncreaseSizeZ

  END FUNCTION
#endif

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Setting   ===
! =============================================================

! Procedure 1 : set the first index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE memorySetFirstIndex(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      CALL memorySetFirstIndexX(i1)

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE memorySetFirstIndexX(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%firstIndexX = i1

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE memorySetFirstIndex(i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Body
!     - - -
      CALL memorySetFirstIndexX(i1)
      CALL memorySetFirstIndexY(i2)

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE memorySetFirstIndexY(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%firstIndexY = i1

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE memorySetFirstIndex(i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Body
!     - - -
      CALL memorySetFirstIndexX(i1)
      CALL memorySetFirstIndexY(i2)
      CALL memorySetFirstIndexZ(i3)

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE memorySetFirstIndexZ(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%firstIndexZ = i1

  END SUBROUTINE
#endif

! Procedure 2 : set the last index of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE memorySetLastIndex(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      CALL memorySetLastIndexX(i1)

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE memorySetLastIndexX(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%lastIndexX = i1

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE memorySetLastIndex(i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Body
!     - - -
      CALL memorySetLastIndexX(i1)
      CALL memorySetLastIndexY(i2)

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE memorySetLastIndexY(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%lastIndexY = i1

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE memorySetLastIndex(i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Body
!     - - -
      CALL memorySetLastIndexX(i1)
      CALL memorySetLastIndexY(i2)
      CALL memorySetLastIndexZ(i3)

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE memorySetLastIndexZ(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%lastIndexZ = i1

  END SUBROUTINE
#endif

! Procedure 3 : set the dimension of the array
! ----------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE memorySetSize(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      CALL memorySetSizeX(i1)

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE memorySetSizeX(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%nbOfDataX = i1

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE memorySetSize(i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Body
!     - - -
      CALL memorySetSizeX(i1)
      CALL memorySetSizeY(i2)

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE memorySetSizeY(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%nbOfDataY = i1

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE memorySetSize(i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Body
!     - - -
      CALL memorySetSizeX(i1)
      CALL memorySetSizeY(i2)
      CALL memorySetSizeZ(i3)

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE memorySetSizeZ(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%nbOfDataZ = i1

  END SUBROUTINE
#endif

! Procedure 4 : set the allocated dimension of the array
! ------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE memorySetAllocatedSize(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      CALL memorySetAllocatedSizeX(i1)

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE memorySetAllocatedSizeX(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%allocatedSizeX = i1

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE memorySetAllocatedSize(i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Body
!     - - -
      CALL memorySetAllocatedSizeX(i1)
      CALL memorySetAllocatedSizeY(i2)

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE memorySetAllocatedSizeY(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%allocatedSizeY = i1

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE memorySetAllocatedSize(i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Body
!     - - -
      CALL memorySetAllocatedSizeX(i1)
      CALL memorySetAllocatedSizeY(i2)
      CALL memorySetAllocatedSizeZ(i3)

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE memorySetAllocatedSizeZ(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      workingArray%allocatedSizeZ = i1

  END SUBROUTINE
#endif

! Procedure 5 : set the increase dimension of the array
! ------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  SUBROUTINE memorySetIncreaseSize(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)

  END SUBROUTINE
#endif

#ifdef _ARRAY_1D_DEFINITION_
  SUBROUTINE memorySetIncreaseSizeX(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      INTEGER :: finalValue

!     Body
!     - - -
      finalValue = i1

      IF ( finalValue < 0 ) THEN
         finalValue = memoryGetDefaultIncreaseSizeX()
      END IF

      workingArray%increaseSizeX = finalValue

  END SUBROUTINE
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  SUBROUTINE memorySetIncreaseSize(i1,i2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)

  END SUBROUTINE
#endif

#ifdef _ARRAY_2D_DEFINITION_
  SUBROUTINE memorySetIncreaseSizeY(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      INTEGER :: finalValue

!     Body
!     - - -
      finalValue = i1

      IF ( finalValue < 0 ) THEN
         finalValue = memoryGetDefaultIncreaseSizeY()
      END IF

      workingArray%increaseSizeY = finalValue

  END SUBROUTINE
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  SUBROUTINE memorySetIncreaseSize(i1,i2,i3)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3

!     Body
!     - - -
      CALL memorySetIncreaseSizeX(i1)
      CALL memorySetIncreaseSizeY(i2)
      CALL memorySetIncreaseSizeZ(i3)

  END SUBROUTINE
#endif

#ifdef _ARRAY_3D_DEFINITION_
  SUBROUTINE memorySetIncreaseSizeZ(i1)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      INTEGER :: finalValue

!     Body
!     - - -
      finalValue = i1

      IF ( finalValue < 0 ) THEN
         finalValue = memoryGetDefaultIncreaseSizeZ()
      END IF

      workingArray%increaseSizeZ = finalValue

  END SUBROUTINE
#endif

! Procedure 6 : define the allocation status of the array
! --------------------------------------------------------
   SUBROUTINE memorySetAllocationStatus(icheck)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: icheck

!     Body
!     - - -
      workingArray%isAllocated = icheck

   END SUBROUTINE

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1 : allocated memory to the array
! ---------------------------------------------
  SUBROUTINE memoryAllocateMemory()

!     Declaration
!     - - - - - -
#ifdef _ARRAY_1D_DEFINITION_
      INTEGER :: istartX, iendX
#endif
#ifdef _ARRAY_2D_DEFINITION_
      INTEGER :: istartY, iendY
#endif
#ifdef _ARRAY_3D_DEFINITION_
      INTEGER :: istartZ, iendZ
#endif

!     Body
!     - - -
#ifdef _ARRAY_1D_DEFINITION_
      istartX = memoryGetFirstIndexX()
      iendX = istartX + memoryGetAllocatedSizeX()
#endif
#ifdef _ARRAY_2D_DEFINITION_
      istartY = memoryGetFirstIndexY()
      iendY = istartY + memoryGetAllocatedSizeY()
#endif
#ifdef _ARRAY_3D_DEFINITION_
      istartZ = memoryGetFirstIndexZ()
      iendZ = istartZ + memoryGetAllocatedSizeZ()
#endif

#ifdef _ARRAY_1D_
      ALLOCATE(workingArray%values(istartX:iendX))
#endif
#ifdef _ARRAY_2D_
      ALLOCATE(workingArray%values(istartX:iendX,istartY:iendY))
#endif
#ifdef _ARRAY_3D_
      ALLOCATE(workingArray%values(istartX:iendX,istartY:iendY,istartZ:iendZ))
#endif

      CALL memorySetAllocationStatus(true)

  END SUBROUTINE

! Procedure 2 : first allocated memory to the array
! ---------------------------------------------
  SUBROUTINE memoryFirstAllocateMemory()

!     Body
!     - - -
#ifdef _ARRAY_1D_DEFINITION_
      CALL memorySetAllocatedSizeX(ione)
#endif
#ifdef _ARRAY_2D_DEFINITION_
      CALL memorySetAllocatedSizeY(ione)
#endif
#ifdef _ARRAY_3D_DEFINITION_
      CALL memorySetAllocatedSizeZ(ione)
#endif

      CALL memoryAllocateMemory()

  END SUBROUTINE

! Procedure 3 : deallocation of the memory
! ------------------------------------------
  SUBROUTINE memoryDestructor()

!     Body
!     - - -
      DEALLOCATE(workingArray%values)
      workingArray%values => NULL()

#ifdef _ARRAY_1D_DEFINITION_
      CALL memorySetSizeX(izero)
      CALL memorySetAllocatedSizeX(izero)
      CALL memorySetFirstIndexX(ione)
      CALL memorySetLastIndexX(computeLastIndex(ione,izero))
#endif
#ifdef _ARRAY_2D_DEFINITION_
      CALL memorySetSizeY(izero)
      CALL memorySetAllocatedSizeY(izero)
      CALL memorySetFirstIndexY(ione)
      CALL memorySetLastIndexY(computeLastIndex(ione,izero))
#endif
#ifdef _ARRAY_3D_DEFINITION_
      CALL memorySetSizeZ(izero)
      CALL memorySetAllocatedSizeZ(izero)
      CALL memorySetFirstIndexZ(ione)
      CALL memorySetLastIndexZ(computeLastIndex(ione,izero))
#endif

      CALL memorySetAllocationStatus(false)

  END SUBROUTINE

! Procedure 4 : define the array
! ---------------------------------
   SUBROUTINE memoryArrayDefine()

!     Body
!     - - -
#ifdef _ARRAY_1D_DEFINITION_
      CALL memorySetFirstIndexX(defaultFirstIndexX)
      CALL memorySetSizeX(izero)
      CALL memorySetLastIndexX(computeLastIndex(defaultFirstIndexX,izero))
      CALL memorySetIncreaseSizeX(memoryGetDefaultIncreaseSizeX())
      CALL memorySetAllocatedSizeX(memoryGetIncreaseSizeX())
#endif
#ifdef _ARRAY_2D_DEFINITION_
      CALL memorySetFirstIndexY(defaultFirstIndexY)
      CALL memorySetSizeY(izero)
      CALL memorySetLastIndexY(computeLastIndex(defaultFirstIndexY,izero))
      CALL memorySetIncreaseSizeY(memoryGetDefaultIncreaseSizeY())
      CALL memorySetAllocatedSizeY(memoryGetIncreaseSizeY())
#endif
#ifdef _ARRAY_3D_DEFINITION_
      CALL memorySetFirstIndexZ(defaultFirstIndexZ)
      CALL memorySetSizeZ(izero)
      CALL memorySetLastIndexZ(computeLastIndex(defaultFirstIndexZ,izero))
      CALL memorySetIncreaseSizeZ(memoryGetDefaultIncreaseSizeZ())
      CALL memorySetAllocatedSizeZ(memoryGetIncreaseSizeZ())
#endif

      CALL memorySetAllocationStatus(false)

   END SUBROUTINE

! Procedure 5 : compute last index
! --------------------------------
  FUNCTION computeLastIndex(firstIndex,size) RESULT(lastIndex)

!     Déclaration
!     - - - - - -
      INTEGER, INTENT(IN) :: firstIndex, size
      INTEGER :: lastIndex

!     Body
!     - - -
      lastIndex = size + firstIndex - 1

  END FUNCTION

! Procedure 6 : define last index
! -------------------------------
  SUBROUTINE memoryDefineLastIndex()

!     Body
!     - - -
#ifdef _ARRAY_1D_DEFINITION_
      CALL memorySetLastIndexX(computeLastIndex(memoryGetFirstIndexX(),memoryGetSizeX()))
#endif
#ifdef _ARRAY_2D_DEFINITION_
      CALL memorySetLastIndexY(computeLastIndex(memoryGetFirstIndexY(),memoryGetSizeY()))
#endif
#ifdef _ARRAY_3D_DEFINITION_
      CALL memorySetLastIndexZ(computeLastIndex(memoryGetFirstIndexZ(),memoryGetSizeZ()))
#endif

  END SUBROUTINE


#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

END MODULE moduleMemoryArrayManagement


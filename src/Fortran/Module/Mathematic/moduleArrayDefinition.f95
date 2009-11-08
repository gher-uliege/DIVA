MODULE moduleArrayDefinition

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

! Declaration
! ===========
!  Array type
!  -----------
   TYPE arrayType

!  1) Data related to memory management
!  ------------------------------------
!     1.1) Flag for memory allocation status
!     + + + + + + + + + + + + + + + + + + + +
       LOGICAL :: isAllocated   ! true if array memory is allocated, false if not

!     1.2) Allocated dimension for 1D array and increase size for reallocation
!     + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
#ifdef _ARRAY_1D_DEFINITION_
       INTEGER :: allocatedSizeX
       INTEGER :: increaseSizeX
#endif

!     1.3) Allocated dimension for 2D array and increase size for reallocation (in complement to dimension for 1D array)
!     + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
#ifdef _ARRAY_2D_DEFINITION_
       INTEGER :: allocatedSizeY
       INTEGER :: increaseSizeY
#endif

!     1.4) Allocated dimension for 3D array and increase size for reallocation (in complement to dimension for 2D array)
!     + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
#ifdef _ARRAY_3D_DEFINITION_
       INTEGER :: allocatedSizeZ
       INTEGER :: increaseSizeZ
#endif


!  2) Array dimension
!  ------------------
!     2.1) Dimension for 1D array
!     + + + + + + + + + + + + + + +
#ifdef _ARRAY_1D_DEFINITION_
       INTEGER :: nbOfDataX
#endif

!     2.2) Dimension for 2D array (in complement to dimension for 1D array)
!     + + + + + + + + + + + + + + +
#ifdef _ARRAY_2D_DEFINITION_
       INTEGER :: nbOfDataY
#endif

!     2.3) Dimension for 3D array (in complement to dimension for 2D array)
!     + + + + + + + + + + + + + + +
#ifdef _ARRAY_3D_DEFINITION_
       INTEGER :: nbOfDataZ
#endif

!  3) First and last index
!  -----------------------
!     3.1) For 1D array
!     + + + + + + + + + +
#ifdef _ARRAY_1D_DEFINITION_
       INTEGER :: firstIndexX
       INTEGER :: lastIndexX
#endif

!     3.2) For 2D array
!     + + + + + + + + + +
#ifdef _ARRAY_2D_DEFINITION_
       INTEGER :: firstIndexY
       INTEGER :: lastIndexY
#endif

!     3.3) For 3D array
!     + + + + + + + + + +
#ifdef _ARRAY_3D_DEFINITION_
       INTEGER :: firstIndexZ
       INTEGER :: lastIndexZ
#endif


!  4) Values storage
!  -----------------

!     4.1) For 1D array
!     + + + + + + + + + +
#ifdef _ARRAY_1D_
       VARType, DIMENSION(:), POINTER :: values
#endif

!     4.2) For 2D array
!     + + + + + + + + + +
#ifdef _ARRAY_2D_
       VARType, DIMENSION(:,:), POINTER :: values
#endif

!     4.3) For 3D array
!     + + + + + + + + + +
#ifdef _ARRAY_3D_
       VARType, DIMENSION(:,:,:), POINTER :: values
#endif

   END TYPE

#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

END MODULE moduleArrayDefinition


MODULE templatearrayType

#define _ARRAY_2D_DEFINITION_
#define _ARRAY_3D_DEFINITION_

#ifdef _ARRAY_2D_
#undef _ARRAY_3D_DEFINITION_
#endif


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

! Declaration
! ===========
!  array type
!  -----------
   TYPE arrayType
   
       LOGICAL :: isAllocated

       INTEGER :: nbOfDataX
       INTEGER :: allocatedSizeX
       INTEGER :: startValueX
       INTEGER :: increaseSizeX

#ifdef _ARRAY_2D_DEFINITION_
       INTEGER :: nbOfDataY
       INTEGER :: allocatedSizeY
       INTEGER :: startValueY
       INTEGER :: increaseSizeY
#endif
#ifdef _ARRAY_3D_DEFINITION_
       INTEGER :: nbOfDataZ
       INTEGER :: allocatedSizeZ
       INTEGER :: startValueZ
       INTEGER :: increaseSizeZ
#endif

#ifdef _ARRAY_2D_
       VARType, DIMENSION(:,:), POINTER :: values
#endif
#ifdef _ARRAY_3D_
       VARType, DIMENSION(:,:,:), POINTER :: values
#endif

   END TYPE
   
END MODULE templatearrayType


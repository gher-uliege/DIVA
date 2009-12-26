MODULE moduleValuesDataBaseManagement

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
#define _DB_1D_DEFINITION_
#define _DB_2D_DEFINITION_
#define _DB_3D_DEFINITION_

#ifdef _DB_1D_
#undef _DB_2D_DEFINITION_
#undef _DB_3D_DEFINITION_
#endif

#ifdef _DB_2D_
#undef _DB_3D_DEFINITION_
#endif

! Include file
! ============
   USE moduleGenericTypeDefinition
   USE moduleWorkingDataBase, ONLY : workingDataBase

! Declaration
! ===========

!  General part
!  ------------


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryGetValues, memoryGetPointerOnValue

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

! Procedure 1 : get reference to pointer containing the values
! ------------------------------------------------------------
   FUNCTION memoryGetValues() RESULT(ptr)

!    Declaration
!    - - - - - - -

!        1) For 1D array
!        + + + + + + + + +
#ifdef _DB_1D_
       TYPE(genericType), DIMENSION(:), POINTER :: ptr
#endif

!        2) For 2D array
!        + + + + + + + +
#ifdef _DB_2D_
       TYPE(genericType), DIMENSION(:,:), POINTER :: ptr
#endif

!        3) For 3D array
!        + + + + + + + +
#ifdef _DB_3D_
       TYPE(genericType), DIMENSION(:,:,:), POINTER :: ptr
#endif


!     Body
!     - - -
      ptr => workingDataBase%values

   END FUNCTION


! Procedure 2 : get a pointer on the value in the array at specified position
! ----------------------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _DB_1D_
  FUNCTION memoryGetPointerOnValue(i1) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      TYPE(genericType), POINTER :: ptr

!     Body
!     - - -
      ptr => workingDataBase%values(i1)

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _DB_2D_
  FUNCTION memoryGetPointerOnValue(i1,i2) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
      TYPE(genericType), POINTER :: ptr

!     Body
!     - - -
      ptr => workingDataBase%values(i1,i2)

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _DB_3D_
  FUNCTION memoryGetPointerOnValue(i1,i2,i3) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2,i3
      TYPE(genericType), POINTER :: ptr

!     Body
!     - - -
      ptr => workingDataBase%values(i1,i2,i3)

  END FUNCTION
#endif



#undef _DB_1D_DEFINITION_
#undef _DB_2D_DEFINITION_
#undef _DB_3D_DEFINITION_

END MODULE moduleValuesDataBaseManagement


MODULE moduleValuesArrayManagement

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

! Declaration
! ===========

!  General part
!  ------------


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: memoryGetValues, memoryGetValue, memoryGetPointerOnValue

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
#ifdef _ARRAY_1D_
       VARType, DIMENSION(:), POINTER :: ptr
#endif

!        2) For 2D array
!        + + + + + + + +
#ifdef _ARRAY_2D_
       VARType, DIMENSION(:,:), POINTER :: ptr
#endif

!        3) For 3D array
!        + + + + + + + +
#ifdef _ARRAY_3D_
       VARType, DIMENSION(:,:,:), POINTER :: ptr
#endif


!     Body
!     - - -
      ptr => workingArray%values

   END FUNCTION

! Procedure 2 : get the value in the array at specified position
! ---------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetValue(i1) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType :: val

!     Body
!     - - -
      val = workingArray%values(i1)

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  FUNCTION memoryGetValue(i1,i2) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2
      VARType :: val

!     Body
!     - - -
      val = workingArray%values(i1,i2)

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  FUNCTION memoryGetValue(i1,i2,i3) RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1, i2, i3
      VARType :: val

!     Body
!     - - -
      val = workingArray%values(i1,i2,i3)

  END FUNCTION
#endif


! Procedure 3 : get a pointer on the value in the array at specified position
! ----------------------------------------------------------------------------
!    Procedure A : for 1D array
!    ----------------------------
#ifdef _ARRAY_1D_
  FUNCTION memoryGetPointerOnValue(i1) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values(i1)

  END FUNCTION
#endif

!    Procedure B : for 2D array
!    ----------------------------
#ifdef _ARRAY_2D_
  FUNCTION memoryGetPointerOnValue(i1,i2) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values(i1,i2)

  END FUNCTION
#endif

!    Procedure C : for 3D array
!    ----------------------------
#ifdef _ARRAY_3D_
  FUNCTION memoryGetPointerOnValue(i1,i2,i3) RESULT(ptr)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: i1,i2,i3
      VARType, POINTER :: ptr

!     Body
!     - - -
      ptr => workingArray%values(i1,i2,i3)

  END FUNCTION
#endif



#undef _ARRAY_1D_DEFINITION_
#undef _ARRAY_2D_DEFINITION_
#undef _ARRAY_3D_DEFINITION_

END MODULE moduleValuesArrayManagement


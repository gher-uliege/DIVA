MODULE moduleMathematicArray3D

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
   USE moduleMemoryArrayManagement, ONLY : memoryGetFirstIndexX, memoryGetFirstIndexY, memoryGetFirstIndexZ, &
                                           memoryGetLastIndexX, memoryGetLastIndexY, memoryGetLastIndexZ
   USE moduleValuesArrayManagement, ONLY : memoryGetValues

   INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
#ifdef _REAL_
   PUBLIC :: mathArrayNorm1, mathArrayNorm2, mathArrayNormInfinity, mathArraySqrt, mathArraySum, &
             mathArrayScale
#endif

   PUBLIC :: mathArrayMin, mathArrayMax, mathArrayAbsMin, mathArrayAbsMax

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

#ifdef _REAL_
! Procedure 1 : norm1 = sum(abs(xi))
! ----------------------------------
  FUNCTION mathArrayNorm1() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = sum(abs(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)))

  END FUNCTION

! Procedure 2 : norm2 = sqrt(sum(xi^2))
! ----------------------------------
  FUNCTION mathArrayNorm2() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()
      val = sqrt(sum(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)*ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)))

  END FUNCTION

! Procedure 3 : norminf = maxval(abs(xi))
! ---------------------------------------
  FUNCTION mathArrayNormInfinity() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = maxval(abs(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)))

  END FUNCTION

! Procedure 4 : sum(xi)
! ---------------------
  FUNCTION mathArraySum() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = sum(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ))

  END FUNCTION

! Procedure 5 : make sqrt of all values
! -------------------------------------
  SUBROUTINE mathArraySqrt()

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType, DIMENSION(:,:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr1 =>  memoryGetValues()

      ptr1(istartX:iendX,istartY:iendY,istartZ:iendZ) = sqrt(ptr1(istartX:iendX,istartY:iendY,istartZ:iendZ))

  END SUBROUTINE

! Procedure 6 : scale the array values
! -------------------------------------
  SUBROUTINE mathArrayScale(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr1

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr1 => memoryGetValues()

      ptr1(istartX:iendX,istartY:iendY,istartZ:iendZ) = val * ptr1(istartX:iendX,istartY:iendY,istartZ:iendZ)

  END SUBROUTINE

#endif

! Procedure 7 : min value
! -----------------------
  FUNCTION mathArrayMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = minval(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ))

  END FUNCTION

! Procedure 8 : max value
! -----------------------
  FUNCTION mathArrayMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER ::istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = maxval(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ))

  END FUNCTION

! Procedure 9 : min value
! -----------------------
  FUNCTION mathArrayAbsMin() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = minval(abs(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)))

  END FUNCTION

! Procedure 10 : max value
! -----------------------
  FUNCTION mathArrayAbsMax() RESULT(val)

!     Declaration
!     - - - - - -
      INTEGER :: istartX, iendX, istartY, iendY, istartZ, iendZ
      VARType :: val
      VARType, DIMENSION(:,:,:), POINTER :: ptr

!     Body
!     - - -
      istartX = memoryGetFirstIndexX()
      iendX = memoryGetLastIndexX()
      istartY = memoryGetFirstIndexY()
      iendY = memoryGetLastIndexY()
      istartZ = memoryGetFirstIndexZ()
      iendZ = memoryGetLastIndexZ()

      ptr =>  memoryGetValues()

      val = maxval(abs(ptr(istartX:iendX,istartY:iendY,istartZ:iendZ)))

  END FUNCTION

END MODULE moduleMathematicArray3D

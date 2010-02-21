MODULE moduleSort

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

! Procedure 1 : sorting procedure
! --------------------------------
SUBROUTINE QS2I1R (IA,JA,N)

!=============================================================================
! *** DESCRIPTION (from www.netlib.org)
!     Written by Rondall E Jones
!     Modified by John A. Wisniewski to use the Singleton QUICKSORT
!     algorithm. date 18 November 1976.
!
!     Further modified by David K. Kahaner
!     National Bureau of Standards
!     August, 1981
!
!     Even further modification made to bring the code up to the
!     Fortran 77 level and make it more readable and to carry
!     along one integer array and one REAL(KIND=8) ::  array during the sort by
!     Mark K. Seager
!     Lawrence Livermore National Laboratory
!     November, 1987
!     This routine was adapted from the ISORT routine.
!
!     ABSTRACT
!         This routine sorts an  array IA and makes the same
!         interchanges in the integer array JA
!         The array IA may be sorted in increasing order
!         A slightly modified quicksort algorithm is used.
!
!     DESCRIPTION OF PARAMETERS
!        IA -  array of values to be sorted.
!        JA - Integer array to be carried along.
!
!         N - Number of values in integer array IA to be sorted.
!
!     .. Scalar Arguments ..

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) ::  N
      REAL(KIND=8), DIMENSION(:), POINTER :: IA
      INTEGER(KIND=4), DIMENSION(:), POINTER :: JA

      REAL(KIND=8) ::  R
      INTEGER(KIND=4) ::  I,  IJ,  J, JJT, JT, K, L, M,NN
      REAL(KIND=8) ::    IIT,  IT
      INTEGER(KIND=4) ::  IL(21), IU(21)

!     Body
!     - - -

!C --- FIRST EXECUTABLE STATEMENT  QS2I1R ---
      NN=N
      IF ( N == 1 ) THEN
          PRINT*, 'No need to sort a single data point'
      RETURN
      ENDIF

!C     Sort IA and carry JA and A along.
!C     And now...Just a little black magic...

      M = 1
      I = 1
      J = NN
      R = .375E0
 210  IF( R.LE.0.5898437E0 ) THEN
         R = R + 3.90625E-2
      ELSE
         R = R-.21875E0
      ENDIF
 225  K = I

!C     Select a central element of the array and save it in location
!C     it, jt, at.
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)

!C     If first element of array is greater than it, interchange with it.
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
      ENDIF
      L=J

!C     If last element of array is less than it, swap with it.
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)

!C     If first element of array is greater than it, swap with it.
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
         ENDIF
      ENDIF

!C     Find an element in the second half of the array which is
!C     smaller than it.
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240

!C     Find an element in the first half of the array which is
!C     greater than it.
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245

!C     Interchange these elements.
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         GOTO 240
      ENDIF

!C     Save upper and lower subscripts of the array yet to be sorted.
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260

!C     Begin again on another portion of the unsorted array.
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      GO TO 265

 300  CONTINUE

END SUBROUTINE

END MODULE moduleSort

MODULE templateJacobiSolver

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
   USE basicLinearSolver

! Declaration
! ===========

!  General part
!  ------------
   VARType, PRIVATE, PARAMETER :: diagonalTolerance = 0.0000000001

!  Memory part
!  -----------

! Procedures status
! =================

  PRIVATE :: internalSolve, checkDimension, checkZeroOnDiagonal, getDiagonalTolerance
  
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


! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================

!  1) procedure to solve linear system with Jacobi method
!  ------------------------------------------------------
   SUBROUTINE internalSolve()
   
!     Declaration
!     - - - - - -
      TYPE(vectorType) :: diagonalVector
      TYPE(vectorType) :: residu
      INTEGER :: errorCheck, size, maxNbOfIteration
      INTEGER :: i1
      VARType :: tolerance, normInitialResidu, normResidu, normalizedResiduNorm
      
      VARType, DIMENSION(:), POINTER :: secondMemberValues, solutionValues, residuValues, diagonalVectorValues
      VARType, DIMENSION(:,:), POINTER :: workingMatrixValues

!     Body
!     - - -
!        Check if dimensions are correct
!        - - - - - - - - - - - - - - - -
      errorCheck =  checkDimension()
      
      IF ( errorCheck /= izero ) THEN
         CALL linearSolverSetError(errorCheck)
         RETURN
      END IF
      
!        Get the diagonal of the matrix
!        - - - - - - - - - - - - - - - -
      size = matrixGetSizeX(workingMatrix)
      
      CALL vectorCreateBase(diagonalVector)
      CALL vectorSetIncreaseSize(diagonalVector,izero)
      CALL vectorSetSize(diagonalVector,size)
      CALL matrixExtractDiagonal(workingMatrix,diagonalVector)
      
      diagonalVectorValues => vectorGetValues(diagonalVector)
      workingMatrixValues => matrixGetValues(workingMatrix)
      
!        Check if diagonal contains "zero" value
!        - - - - - - - - - - - - - - - - - - - -
      errorCheck = checkZeroOnDiagonal(diagonalVector)

      IF ( errorCheck /= izero ) THEN
         CALL linearSolverSetError(errorCheck)
         RETURN
      END IF

!        Compute solution
!        - - - - - - - - -
      tolerance = getConvergenceTolerance()
      
      CALL vectorCreateBase(residu)
      CALL vectorSetIncreaseSize(residu,izero)
      CALL vectorSetSize(residu,size)
      residuValues => vectorGetValues(residu)
      
      secondMemberValues => vectorGetValues(secondMember)
      solutionValues => vectorGetValues(solution)

      CALL initialiseSolution()
      CALL computeResidu(secondMemberValues,workingMatrixValues,solutionValues,size,residuValues,normInitialResidu)

      errorCheck = ithree
      DO i1 = 1, maxNbOfIteration
         CALL computeSolution(residuValues,diagonalVectorValues,solutionValues,size)
         CALL computeResidu(secondMemberValues,workingMatrixValues,solutionValues,size,residuValues,normResidu)
         normalizedResiduNorm = normResidu / normInitialResidu
         IF ( normalizedResiduNorm < tolerance ) THEN
            errorCheck = izero
            GOTO 100
         END IF
      END DO

      CALL linearSolverSetError(errorCheck)
      CALL linearIterativeSolverSetResiduNorm(normInitialResidu,normalizedResiduNorm)

      CALL vectorDestroy(diagonalVector)
      CALL vectorDestroy(residu)

   END SUBROUTINE

!  2) Check if dimension of the matrix and vectors are compatible
!  ---------------------------------------------------------------
   FUNCTION checkDimension() RESULT(ierr)
   
!     Declaration
!     - - - - - -
      INTEGER :: matrixNbOfColumn, matrixNbOfRow
      INTEGER :: ierr

!     Body
!     - - -
      ierr = izero
      matrixNbOfColumn = matrixGetSizeY(workingMatrix)
      matrixNbOfRow = matrixGetSizeX(workingMatrix)

      IF ( vectorGetSize(secondMember) /= matrixNbOfColumn ) THEN
         ierr = ione
         RETURN
      END IF
      
      IF ( vectorGetSize(solution) /= matrixNbOfRow ) THEN
         ierr = ione
         RETURN
      END IF

      IF ( matrixNbOfRow /= matrixNbOfColumn ) THEN
         ierr = ione
         RETURN
      END IF

   END FUNCTION

!  3) Check if diagonal contains "zero" value
!  ------------------------------------------
   FUNCTION checkZeroOnDiagonal(diagonalVector) RESULT(ierr)

!     Declaration
!     - - - - - -
      TYPE(vectorType), INTENT(IN) :: diagonalVector
      INTEGER :: ierr

!     Body
!     - - -
      ierr = izero

      IF ( vectorAbsMin(diagonalVector) <= getDiagonalTolerance() ) THEN
         ierr = itwo
      END IF

   END FUNCTION
   
!  4) get the tolerance on the diagonal value
!  ------------------------------------------
   FUNCTION getDiagonalTolerance() RESULT(tolerance)
   
!     Declaration
!     - - - - - -
      VARType :: tolerance

!     Body
!     - - -
      tolerance = diagonalTolerance

   END FUNCTION
   
END MODULE templateJacobiSolver


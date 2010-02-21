PROGRAM UVtopogen

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE ioInterface
  USE array3DInterface
  USE moduleCoordinateInformationReal4, initialiseCoordinateInformation => initialise

  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   TYPE(file) :: inputFile1, inputFile2, inputFile3
   TYPE(file) :: outputFile1, outputFile2
   TYPE(arrayReal4) :: topology
   INTEGER :: nbOfGridPointX, nbOfGridPointY, inputFileUnit, iMax1, jMax1, changeCoordinate, layerNumber
   REAL(KIND=4) :: gridOriginX, gridOriginY, gridStepX, gridStepY, exclusionValueTopology, deltaXInKm, deltaYInKm, &
                   depthValue
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrTopology
   CHARACTER(LEN=4) :: nameFieldUBase, nameFieldVBase
   CHARACTER(LEN=10) :: nameFieldU, nameFieldV
   CHARACTER(LEN=5) :: depth

! ==================
! ==================
! == Main program ==
! ==================
! ==================

!  Always start the DIVA context
!  =============================
   CALL createDIVAContext()
   CALL initialiseCoordinateInformation()

   nameFieldUBase = 'Uvel'
   nameFieldVBase = 'Vvel'

!  Body
!  ====
#ifdef _BATCH_MODE_
#undef _INTERACTIVE_MODE_
#endif

!     1) Creation of array
!     --------------------
   CALL arrayCreate(topology)

!     2) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.10',formType=STD_FORMATTED)
   CALL createFile(inputFile2,'fort.12',formType=GHER_UNFORMATTED)
   CALL createFile(inputFile3,'fort.13',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'notUse',formType=GHER_UNFORMATTED)
   CALL createFile(outputFile2,'notUse',formType=GHER_UNFORMATTED)

!     3) Reading Grid
!     ---------------
   CALL openFile(inputFile1)
   inputFileUnit = getFileUnit(inputFile1)

   READ(inputFileUnit,*) gridOriginX
   READ(inputFileUnit,*) gridOriginY
   READ(inputFileUnit,*) gridStepX
   READ(inputFileUnit,*) gridStepY
   READ(inputFileUnit,*) nbOfGridPointX
   READ(inputFileUnit,*) nbOfGridPointY

   CALL closeFile(inputFile1)

!     4) Reading topology
!     -------------------
   PRINT*,'into ureadc', nbOfGridPointX, nbOfGridPointY
   CALL arrayRead(topology,inputFile2,exclusionValueTopology)
   ptrTopology => arrayGetValues(topology)

   iMax1 = arrayGetSizeX(topology)
   jMax1 = arrayGetSizeY(topology)

   PRINT*,'out of reading',iMax1,jMax1

   IF ( ( iMax1 /= nbOfGridPointX ).OR.( jMax1 /= nbOfGridPointY )) THEN
      STOP 'incoherent files'
   ENDIF

!     5) Change coordinate if needed
!     ------------------------------
!       5.1) In interactive mode
!       - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Change coordinate (1) or not (0)'
   READ(stdInput,*) changeCoordinate

!       5.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) changeCoordinate
30 CONTINUE

#endif

!       5.3) Computing change
!       - - - - - - - - - - -
    CALL setIChangeCoordinate(changeCoordinate)

    IF ( getIChangeCoordinate() == 1 ) THEN

      CALL setMinimumLongitude(gridOriginX)
      CALL setMaximumLongitude(gridOriginX+(nbOfGridPointX-1)*gridStepX)
      CALL setMinimumLatitude(gridOriginY)
      CALL setMaximumLatitude(gridOriginY+(nbOfGridPointY-1)*gridStepY)

      CALL computeMeanLongitude()
      CALL computeMeanLatitude()

      deltaYInKm = ( 4. * asin(1.) * 6360. ) / 360.
      deltaXInKm = asin(1.) * getMeanLatitude() / 90.
      deltaXInKm = 6360. * cos( deltaXInKm )
      deltaXInKm = ( 4. * asin(1.) * deltaXInKm ) / 360.

      CALL setDeltaXInKm(deltaXInKm)
      CALL setDeltaYInKm(deltaYInKm)

      gridStepX = gridStepX * deltaXInKm
      gridStepY = gridStepY * deltaYInKm

    ENDIF

!     6) Computing field U and V
!     --------------------------

   CALL openFile(inputFile3)
   inputFileUnit = getFileUnit(inputFile3)

   depthValue = 0.
   layerNumber = 0

1  CONTINUE
   READ(inputFileUnit,*,END=99,ERR=99) depthValue
   layerNumber = layerNumber + 1
   WRITE(depth,88) 10000 + layerNumber

   nameFieldU = nameFieldUBase//'.'//depth
   nameFieldV = nameFieldVBase//'.'//depth

   CALL defineFileName(outputFile1,nameFieldU)
   CALL defineFileName(outputFile2,nameFieldV)

   CALL uvg(ptrTopology(1:iMax1,1:jMax1,1),depthValue,nbOfGridPointX,nbOfGridPointY,gridStepX,gridStepY, &
            exclusionValueTopology,outputFile1,outputFile2)

   GOTO 1
99 CONTINUE
   CALL closeFile(inputFile3)

88 FORMAT(I5)

!  Always finalise the DIVA context
!  ================================
   CALL arrayDestroy(topology)
   CALL finaliseDIVAContext()

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Program procedures                  ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS

! Procedure 1 : uvg
! -----------------
SUBROUTINE uvg(topology,depthValue,nbOfGridPointX,nbOfGridPointY,gridStepX,gridStepY, &
               exclusionValue,outputFile1,outputFile2)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfGridPointX,nbOfGridPointY
      REAL(KIND=4), INTENT(IN) :: exclusionValue, depthValue, gridStepX,gridStepY
      REAL(KIND=4), DIMENSION(:,:), TARGET :: topology
      TYPE(file) :: outputFile1, outputFile2

      TYPE(arrayReal4) :: fieldU, fieldV
      REAL(KIND=4) :: meanDepth, minDepth, depth, xi, factor
      REAL(KIND=4), POINTER :: ptrTopologyValue
      INTEGER :: i1, i2, iMean, iMin, nbOfSmoothLoop
      REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrFieldU, ptrFieldV

!     Body
!     - - -

!       1) Create matrices
!       + + + + + + + + + +
      CALL arrayCreate(fieldU,nbOfGridPointX,nbOfGridPointY,1)
      CALL arrayCreate(fieldV,nbOfGridPointX,nbOfGridPointY,1)

!       2) Compute mean depth
!       + + + + + + + + + + +
      meanDepth = 0.
      iMean = 0
      minDepth = 0.
      iMin = 0

      DO i2 = 1, nbOfGridPointY
       DO i1 = 1, nbOfGridPointX
        ptrTopologyValue => topology(i1,i2)
        IF ( ptrTopologyValue > 0. ) THEN
          meanDepth = meanDepth + ptrTopologyValue
          iMean = iMean + 1
        ELSE
          minDepth = minDepth - ptrTopologyValue
          iMin = iMin + 1
        ENDIF
       ENDDO
      ENDDO

      IF ( iMean > 0 ) THEN
         depth = 0.2 * meanDepth / iMean
      ELSE
         depth = 0.2 * minDepth / iMin
      ENDIF

!       3) Evaluate centered gradients
!       + + + + + + + + + + + + + + + +

      ptrFieldU => arrayGetValues(fieldU)
      ptrFieldV => arrayGetValues(fieldV)

!$OMP PARALLEL DEFAULT(NONE) SHARED(nbOfGridPointX,nbOfGridPointY,depthValue,topology,depth, &
!$OMP                               gridStepX,gridStepY,ptrFieldU, ptrFieldV) &
!$OMP                        PRIVATE(i1,i2,xi,factor)
!$OMP DO

      DO i2 = 2, nbOfGridPointY - 1
       DO i1 = 2, nbOfGridPointX - 1
          xi = ( depthValue - topology(i1,i2) ) / depth
          factor = 0.

          IF ( abs(xi) < 5. ) THEN
             factor = exp((-1.)*xi*xi)
          ENDIF

          ptrFieldU(i1,i2,1) = ( topology(i1,i2+1) - topology(i1,i2-1) ) * factor / gridStepY
          ptrFieldV(i1,i2,1) = ( topology(i1-1,i2) - topology(i1+1,i2) ) * factor / gridStepX

       ENDDO
      ENDDO

!$OMP END DO
!$OMP END PARALLEL
      
!       4) Fill boundaries
!       + + + + + + + + + +
     ptrFieldU(1:nbOfGridPointX,1,1) = ptrFieldU(1:nbOfGridPointX,2,1)
     ptrFieldU(1:nbOfGridPointX,nbOfGridPointY,1) = ptrFieldU(1:nbOfGridPointX,nbOfGridPointY-1,1)
     ptrFieldU(1,1:nbOfGridPointY,1) = ptrFieldU(2,1:nbOfGridPointY,1)
     ptrFieldU(nbOfGridPointX,1:nbOfGridPointY,1) = ptrFieldU(nbOfGridPointX-1,1:nbOfGridPointY,1)
      
     ptrFieldV(1:nbOfGridPointX,1,1) = ptrFieldV(1:nbOfGridPointX,2,1)
     ptrFieldV(1:nbOfGridPointX,nbOfGridPointY,1) = ptrFieldV(1:nbOfGridPointX,nbOfGridPointY-1,1)
     ptrFieldV(1,1:nbOfGridPointY,1) = ptrFieldV(2,1:nbOfGridPointY,1)
     ptrFieldV(nbOfGridPointX,1:nbOfGridPointY,1) = ptrFieldV(nbOfGridPointX-1,1:nbOfGridPointY,1)

       
!       5) Smooth field
!       + + + + + + + +

     nbOfSmoothLoop = (sqrt(sqrt((nbOfGridPointX*nbOfGridPointY*1.)+1.)/2.)) + 1

     DO i1 = 1, nbOfSmoothLoop

        ptrFieldU(2:nbOfGridPointX-1,2:nbOfGridPointY-1,1) = 0.25 * ( ptrFieldU(3:nbOfGridPointX,2:nbOfGridPointY-1,1) + &
                                                                      ptrFieldU(1:nbOfGridPointX-2,2:nbOfGridPointY-1,1) + &
                                                                      ptrFieldU(2:nbOfGridPointX-1,3:nbOfGridPointY,1) + &
                                                                      ptrFieldU(2:nbOfGridPointX-1,1:nbOfGridPointY-2,1) )
        ptrFieldV(2:nbOfGridPointX-1,2:nbOfGridPointY-1,1) = 0.25 * ( ptrFieldV(3:nbOfGridPointX,2:nbOfGridPointY-1,1) + &
                                                                      ptrFieldV(1:nbOfGridPointX-2,2:nbOfGridPointY-1,1) + &
                                                                      ptrFieldV(2:nbOfGridPointX-1,3:nbOfGridPointY,1) + &
                                                                      ptrFieldV(2:nbOfGridPointX-1,1:nbOfGridPointY-2,1) )

        ptrFieldU(1:nbOfGridPointX,1,1) = ptrFieldU(1:nbOfGridPointX,2,1)
        ptrFieldU(1:nbOfGridPointX,nbOfGridPointY,1) = ptrFieldU(1:nbOfGridPointX,nbOfGridPointY-1,1)
        ptrFieldU(1,1:nbOfGridPointY,1) = ptrFieldU(2,1:nbOfGridPointY,1)
        ptrFieldU(nbOfGridPointX,1:nbOfGridPointY,1) = ptrFieldU(nbOfGridPointX-1,1:nbOfGridPointY,1)

        ptrFieldV(1:nbOfGridPointX,1,1) = ptrFieldV(1:nbOfGridPointX,2,1)
        ptrFieldV(1:nbOfGridPointX,nbOfGridPointY,1) = ptrFieldV(1:nbOfGridPointX,nbOfGridPointY-1,1)
        ptrFieldV(1,1:nbOfGridPointY,1) = ptrFieldV(2,1:nbOfGridPointY,1)
        ptrFieldV(nbOfGridPointX,1:nbOfGridPointY,1) = ptrFieldV(nbOfGridPointX-1,1:nbOfGridPointY,1)

     ENDDO

!       6) Write result
!       + + + + + + + +
     CALL arrayWrite(fieldU,outputFile1,exclusionValue)
     CALL arrayWrite(fieldV,outputFile2,exclusionValue)


!       7) Destroy matrices
!       + + + + + + + + + +
      CALL arrayDestroy(fieldU)
      CALL arrayDestroy(fieldV)
      
      
END SUBROUTINE

END PROGRAM UVtopogen

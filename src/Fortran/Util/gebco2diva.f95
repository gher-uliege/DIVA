PROGRAM gebcoToDiva

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE matrixInterface

  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: nbOfDataInGebco, reductionAlongX, reductionAlongY, inputFileUnit, iorder, outputFileUnit, &
              nbOfPointAlongX, nbOfPointAlongY
   TYPE(file) :: inputFile1, outputFile1, outputFile2
   
   REAL(KIND=8) :: xValue1, yValue1, dValue, xValue2, yValue2, xValue0, yValue0, dx, dy, xShift, yShift
   TYPE(matrixReal4) :: topology
   INTEGER :: iMax
   REAL(KIND=4), PARAMETER :: exclusionValue = -99999.

! ==================
! ==================
! == Main program ==
! ==================
! ==================

!  Always start the DIVA context
!  =============================
   CALL createDIVAContext()

!  Body
!  ====
#ifdef _BATCH_MODE_
#undef _INTERACTIVE_MODE_
#endif

!     1) Read information
!     -------------------
!       1.1) In interactive mode
!       - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the number of data in gebco file'
   READ(stdInput,*) nbOfDataInGebco
   WRITE(stdOutput,*) 'Please enter the reduction parameter along X'
   READ(stdInput,*) reductionAlongX
   WRITE(stdOutput,*) 'Please enter the reduction parameter along Y'
   READ(stdInput,*) reductionAlongY

!       1.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) nbOfDataInGebco,reductionAlongX,reductionAlongY
30 CONTINUE

#endif
         
!     2) Creation of needed files
!     ---------------------------
   CALL createFile(inputFile1,'fort.10',formType=STD_FORMATTED)
   CALL createFile(outputFile1,'fort.20',formType=STD_FORMATTED)
   CALL createFile(outputFile2,'fort.12',formType=GHER_UNFORMATTED)

         
!     3) Checking data
!     ----------------
   CALL openFile(inputFile1)
   inputFileUnit = getFileUnit(inputFile1) 

   IF ( reductionAlongX /= 1 ) THEN
      PRINT*, 'Try to reduce X grid by',reductionAlongX
   ENDIF
   IF ( reductionAlongY /= 1 ) THEN
      PRINT*, 'Try to reduce Y grid by',reductionAlongY
   ENDIF
      
      
!     4) Guess grid topology
!     ----------------------
   PRINT*,'Try to guess grid topology'
   READ(inputFileUnit,*) xValue1, yValue1, dValue
   READ(inputFileUnit,*) xValue2, yValue2, dValue

   xValue0 = 0.
   yValue0 = 0.
   dx = 0.
   dy = 0.
         
   IF ( abs(xValue2-xValue1) > 1.E-5 * (abs(xValue2)+abs(xValue1)) ) THEN
      dx = xValue2 - xValue1
      xValue0 = xValue1
      PRINT*, 'x coordinates vary first',dx
      iorder = 1      
   ELSE
      dy = yValue2 - yValue1
      yValue0 = yValue1
      PRINT*, 'y coordinates vary first',dy
      iorder = 2      
   ENDIF
      
   iMax = 0
   SELECT CASE (iorder)
      CASE (1)
         CALL computeForFirstOrder(inputFileUnit,iMax,yValue0,dy)
      CASE (2)
         CALL computeForSecondOrder()
   END SELECT
            

!     5) Compute topology
!     ------------------
   CALL matrixCreate(topology)
   CALL computeTopology(inputFileUnit,topology,nbOfDataInGebco,reductionAlongX,reductionAlongY,iMax,dx,dy,xValue0,yValue0, &
                        nbOfPointAlongX,nbOfPointAlongY)

!     6) Write results
!     ----------------
   xShift = 0.
   yShift = 0.
      
   IF ( nbOfDataInGebco == 1 ) THEN
      xShift = ( reductionAlongX - 1 ) * dx
   ENDIF

   CALL openFile(outputFile1)
   outputFileUnit = getFileUnit(outputFile1)
   
   WRITE(outputFileUnit,*) xValue0 + xShift 
   WRITE(outputFileUnit,*) yValue0 + yShift
   WRITE(outputFileUnit,*) dx * reductionAlongX
   WRITE(outputFileUnit,*) dy * reductionAlongY
   WRITE(outputFileUnit,*) nbOfPointAlongX
   WRITE(outputFileUnit,*) nbOfPointAlongY
   
   CALL closeFile(outputFile1)
    
   CALL matrixWrite(topology,outputFile2,exclusionValue)
   PRINT*,'Finished writing binary file'
         
!  Always finalise the DIVA context
!  ================================
   CALL matrixDestroy(topology)
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

! Procedure 1 : computeForFirstOrder
! ----------------------------------
SUBROUTINE computeForFirstOrder(inputFileUnit,iMax,yValue0,dy)

!     Declaration
!     - - - - - -
   INTEGER, INTENT(IN) :: inputFileUnit
   INTEGER, INTENT(OUT) :: iMax
   REAL(KIND=8), INTENT(OUT) :: yValue0, dy
   REAL(KIND=8) :: xValue1, yValue1, dValue, xValue2, yValue2
   REAL(KIND=8), PARAMETER :: tolerance = 1.D-5
   INTEGER :: i1
   

!     Body
!     - - -
   REWIND(inputFileUnit)         
   READ(inputFileUnit,*) xValue1, yValue1, dValue

   i1 = 1
1  CONTINUE
   READ(inputFileUnit,*) xValue2, yValue2, dValue

   IF ( abs(yValue2-yValue1) < tolerance * (abs(yValue1)+abs(yValue2)) )  THEN
      i1 = i1 + 1
      xValue1 = xValue2
      yValue1 = yValue2
      GOTO 1
   ELSE
      iMax = i1
      dy = yValue2 - yValue1
      yValue0 = yValue1
      
      IF ( dy < 0. ) THEN
         PRINT*, 'negative dy, will modify'
         
2        CONTINUE
         READ(inputFileUnit,*,ERR=888,END=888) xValue1, yValue1, dValue
         yValue0 = min(yValue0,yValue1)
         GOTO 2 
                  
888      CONTINUE
         dy = (-1.) * dy
         PRINT*, 'y0', yValue0, dy
      ENDIF
      
   ENDIF

END SUBROUTINE
   
! Procedure 2 : computeForSecondOrder
! ------------------------------------
SUBROUTINE computeForSecondOrder()

!     Body
!     - - -
     STOP 'To be implemented'

END SUBROUTINE
  
! Procedure 3 : computeTopology
! -----------------------------
SUBROUTINE computeTopology(inputFileUnit,topology,nbOfDataInGebco,reductionAlongX,reductionAlongY,iMax,dx,dy,xValue0,yValue0, &
                           nbOfPointAlongX, nbOfPointAlongY)

!     Declaration
!     - - - - - -
   INTEGER, INTENT(IN) :: inputFileUnit, nbOfDataInGebco, reductionAlongX, reductionAlongY, iMax
   INTEGER, INTENT(OUT) :: nbOfPointAlongX, nbOfPointAlongY
   TYPE(matrixReal4), INTENT(INOUT) :: topology
   REAL(KIND=8), INTENT(IN) :: xValue0, yValue0
   REAL(KIND=8) :: dx, dy, xValue1, yValue1
   REAL(KIND=4) :: dValue
   REAL(KIND=8), PARAMETER :: oneOver60 = 1./60., oneOver6000 = 1./6000.
   INTEGER :: i2, i3, i4, i, j, jMax
   INTEGER, PARAMETER :: defaultX = 50, defaultY = 50

!     Body
!     - - -
   REWIND(inputFileUnit)
   
   jMax= 1 
   
!       1) Check dx and dy
!       + + + + + + + + + +
   IF ( abs( dx - oneOver60 ) < oneOver6000 ) THEN
      PRINT*, 'Probably 1 minute grid, rounding'
      dx = oneOver60
   ENDIF
   
   IF ( abs( dy - oneOver60 ) < oneOver6000 ) THEN
      PRINT*, 'Probably 1 minute grid, rounding'
      dy = oneOver60
   ENDIF
   
   i2 = 0
   
!      2) Computing topology
!      + + + + + + + + + + +
  
  CALL matrixSetIncreaseSize(topology,defaultX,defaultY)
  
10 CONTINUE

   READ(inputFileUnit,*,END=999,ERR=999) xValue1, yValue1, dValue
   i2 = i2 + 1
   
   IF ( nbOfDataInGebco /= 0 ) THEN

      jMax = nbOfDataInGebco / iMax
      j = jMax - ( i2 - 1 ) / iMax
      i = i2 - ( jMax - j ) * iMax

   ELSE
   
      i = nint ( (xValue1 - xValue0 ) / dx ) + 1
      j = nint ( (yValue1 - yValue0 ) / dy ) + 1
      
   ENDIF
   
   jMax= max(j,jMax)
   
   nbOfPointAlongX = iMax / reductionAlongX
   nbOfPointAlongY = jMax / reductionAlongY
   
   i3 = ( i - 1 ) / reductionAlongX + 1
   i4 = ( j - 1 ) / reductionAlongY + 1
   
   IF ( i3 > nbOfPointAlongX ) THEN
      GOTO 10
   ENDIF

   IF ( i4 > nbOfPointAlongY ) THEN
      GOTO 10
   ENDIF
   
   CALL matrixSetValue(topology,i3,i4,dValue,INSERT_VALUE)

   GOTO 10
   
999 CONTINUE   

END SUBROUTINE

END PROGRAM gebcoToDiva

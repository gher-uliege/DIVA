PROGRAM datacheck

! Module
! ======
  USE moduleDIVA
  USE moduleFile

! Include file
! ============
   INCLUDE 'constantParameter.h'
   INCLUDE 'ioParameter.h'

! Declaration
! ===========
   INTEGER :: inputFileUnit, outputFileUnit
   REALType    :: xMin,xMax,yMin,yMax
   LOGICAL     :: icheck

   Type(file) :: outputFile, inputFile

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

!    Opening file to read and to write
!    ---------------------------------
   CALL createFile(inputFile,'fort.10',formType=STD_FORMATTED)
   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile)

   CALL createFile(outputFile,'fort.61',formType=STD_FORMATTED)
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

!    Main procedure
!    --------------

   CALL computeMinMax(inputFileUnit,xMin,xMax,yMin,yMax,icheck)
   CALL writeOutputFile(outputFileUnit,xMin,xMax,yMin,yMax,icheck)

   CALL closeFile(inputFile)
   CALL closeFile(outputFile)

!  Always finalise the DIVA context
!  ================================
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

! Procedure 1 : compute minimum and maximum
! ------------------------------------------
 SUBROUTINE computeMinMax(inputFileUnit,xMin,xMax,yMin,yMax,icheck)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit
      INTEGER :: i1, i2, istop, numberOfData

      REALType, INTENT(OUT)   :: xMin,xMax,yMin,yMax
      REALType :: xValue, yValue
      REALType, PARAMETER :: startingMin = 1.D36
      REALType, PARAMETER :: startingMax = -1.D36
      REALType, PARAMETER :: defaultMin = -1.D36
      REALType, PARAMETER :: defaultMax = 1.D36
      
      LOGICAL, INTENT(OUT) :: icheck

!     Body
!     - - -
      xMin = startingMin
      yMin = startingMin
      xmax = startingMax
      ymax = startingMax
      icheck = true

       READ(inputFileUnit,*,ERR=200) numberOfData
       DO i1 = 1, numberOfData
          READ(inputFileUnit,*,ERR=200) istop
          DO i2 = 1, istop
             READ(inputFileUnit,*,ERR=200) xValue, yValue
             xMin = min(xValue,xMin)
             yMin = min(yValue,yMin)
             xMax = max(xValue,xMax)
             yMax = max(yValue,yMax)
          END DO
       END DO
       
       RETURN

 200  CONTINUE
 
      xMin = defaultMin
      yMin = defaultMin
      xMax = defaultMax
      yMax = defaultMax
      icheck = false

 END SUBROUTINE

! Procedure 2 : write output file
! -------------------------------
 SUBROUTINE writeOutputFile(outputFileUnit,xMin,xMax,yMin,yMax,icheck)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: outputFileUnit
      REALType, INTENT(IN)    :: xMin,xMax,yMin,yMax
      LOGICAL, INTENT(IN)     :: icheck

!     Body
!     - - -
#ifdef _INTERACTIVE_MODE_
      IF (icheck) THEN
         WRITE(stdOutput,*) 'Bounding box of contours:'
         WRITE(stdOutput,*) '(',xMin,',',xMax,')x(',yMin,',',yMax,')'
         WRITE(stdOutput,*) 'Eliminating all data points outside this box'
      ELSE
         WRITE(stdOutput,*) 'Contour file seems corrupted'
      END IF
#endif

      WRITE(outputFileUnit,*) xMin
      WRITE(outputFileUnit,*) xMax
      WRITE(outputFileUnit,*) yMin
      WRITE(outputFileUnit,*) yMax

 END SUBROUTINE

END PROGRAM datacheck

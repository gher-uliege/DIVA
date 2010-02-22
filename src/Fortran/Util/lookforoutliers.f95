PROGRAM lookForOutliers

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE moduleSort
  USE vectorInterface

  INCLUDE 'ioParameter.h'

! Declaration
! ===========
   REALType :: exclusionValue
   REALType, PARAMETER :: tolerance = 1.D-10, eps = 0.00001

   TYPE(file) :: inputFile1, inputFile2, inputFile3
   TYPE(file) :: outputFile66, outputFile67

   INTEGER :: nbOfData, nbOfOutliers, i1

   TYPE(vectorReal8) :: sValue, saValue
   TYPE(vectorReal4) :: ddValue, dadValue, edValue, xdValue, ydValue
   TYPE(vectorInteger4) :: iwValue
   REALType ::  mad, med
   INTEGER :: inputFileUnit1, inputFileUnit2, inputFileUnit3, outputFileUnit66, outputFileUnit67
   REALType :: xValue, yValue, dValue, xaValue, yaValue, daValue, xeValue, yeValue, eValue, intermValue
   REAL(KIND=8), DIMENSION(:), POINTER :: ptrSaValue, ptrSValue
   INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrIwValue
   INTEGER(KIND=4) :: ptrIW

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

!     1) Read information about the exclusion value
!     ------------------------------------------
!       1.1) In interactive mode
!       - - - - - - - - - - - -
#ifdef _INTERACTIVE_MODE_
   WRITE(stdOutput,*) 'Please enter the exclusion value'
   READ(stdInput,*) exclusionValue

!       1.2) In batch mode
!       - - - - - - - - -
#else
   READ(stdInput,*,END=30) exclusionValue
30 CONTINUE

#endif

!     2) Create files
!     ---------------
   CALL createFile(inputFile1,'fort.44',formType=STD_FORMATTED)
   CALL createFile(inputFile2,'fort.71',formType=STD_FORMATTED)
   CALL createFile(inputFile3,'fort.76',formType=STD_FORMATTED)
   CALL createFile(outputFile66,'fort.66',formType=STD_FORMATTED)
   CALL createFile(outputFile67,'fort.67',formType=STD_FORMATTED)

   CALL openFile(inputFile1)
   CALL openFile(inputFile2)
   CALL openFile(inputFile3)

   inputFileUnit1 = getFileUnit(inputFile1)
   inputFileUnit2 = getFileUnit(inputFile2)
   inputFileUnit3 = getFileUnit(inputFile3)

!     3) Compute nbOfData
!     -------------------
   nbOfData = computeNumberOfData(inputFileUnit1,inputFileUnit2,inputFileUnit3)

   IF ( nbOfData == 0 ) THEN
      STOP 'Problem in input?'
   ENDIF

   CALL vectorCreate(sValue,nbOfData)
   CALL vectorCreate(saValue,nbOfData)
   CALL vectorCreate(ddValue,nbOfData)
   CALL vectorCreate(dadValue,nbOfData)
   CALL vectorCreate(edValue,nbOfData)
   CALL vectorCreate(xdValue,nbOfData)
   CALL vectorCreate(ydValue,nbOfData)
   CALL vectorCreate(iwValue,nbOfData)

   ptrSValue => vectorGetValues(sValue)
   ptrSaValue => vectorGetValues(saValue)
   ptrIwValue => vectorGetValues(iwValue)

!     4) Getting data
!     ---------------
   REWIND(inputFileUnit1)
   REWIND(inputFileUnit2)
   REWIND(inputFileUnit3)

   DO i1 = 1, nbOfData
      READ(inputFileUnit1,*) xValue, yValue, dValue
      READ(inputFileUnit2,*) xaValue, yaValue, daValue
      READ(inputFileUnit3,*) xeValue, yeValue, eValue

      CALL checkCoherence(xvalue,xeValue,i1)
      CALL checkCoherence(xvalue,xaValue,i1)
      CALL checkCoherence(yvalue,yaValue,i1)
      CALL checkCoherence(yvalue,yeValue,i1)

      IF ( abs(eValue) <= tolerance ) THEN
         eValue = tolerance
      ENDIF

      intermValue = ( dValue - daValue ) / eValue
      ptrSValue(i1) = intermValue
      ptrSaValue(i1) = abs(intermValue)
      ptrIwValue(i1) = i1
      ddValue%values(i1) = dValue
      dadValue%values(i1) = daValue
      edValue%values(i1) = eValue
      xdValue%values(i1) = xValue
      ydValue%values(i1) = yValue

      IF ( abs(daValue-exclusionValue) <= eps * abs(exclusionValue) ) THEN
         intermValue = 0.
         ptrSValue(i1) = intermValue
         ptrSaValue(i1) = abs(intermValue)
         dadValue%values(i1) = dValue
         edValue%values(i1) = 10000. + dValue
      ENDIF

   ENDDO

   CALL closeFile(inputFile1)
   CALL closeFile(inputFile2)
   CALL closeFile(inputFile3)

!     5) Sorting data
!     ---------------
   CALL QS2I1R(ptrSaValue,ptrIwValue,nbOfData)

!     6) Writing output
!     -----------------
   CALL openFile(outputFile66)

   outputFileUnit66 = getFileUnit(outputFile66)

   nbOfOutliers = 0

   DO i1 = nbOfData, 1, -1
      ptrIW = ptrIwValue(i1)
      dValue = ddValue%values(ptrIW)
      daValue = dadValue%values(ptrIW)
      eValue = edValue%values(ptrIW)

      IF ( abs(dValue-daValue) > 2.*eValue ) THEN

       IF ( abs(dValue-daValue) > 3.*eValue ) THEN
           WRITE(outputFileUnit66,1234) ptrSaValue(i1),ptrIW,xdValue%values(ptrIW),ydValue%values(ptrIW),dValue,daValue,eValue
       ENDIF

       nbOfOutliers = nbOfOutliers + 1

      ENDIF

   ENDDO

1234  FORMAT(1X,1E10.4,1X,1I8,5(1X,1E10.4))

   DO i1 = 1, nbOfData
      ptrIwValue(i1) = i1
   ENDDO

!     7) Writing information about outliers
!     -------------------------------------
   IF ( nbOfOutliers > ( 0.05 * nbOfData ) ) THEN
       PRINT*,'There are more outliers than usual :',nbOfOutliers,' out of',nbOfData
       WRITE(outputFileUnit66,*) 'There are more outliers than usual :',nbOfOutliers,' out of',nbOfData
   ENDIF

   IF ( nbOfOutliers == 0 ) THEN
       WRITE(outputFileUnit66,*) 'There are no outliers'
       PRINT*,'There are no outliers'
   ENDIF

   IF ( nbOfOutliers <= ( 0.05 * nbOfData ) ) THEN
       PRINT*, 'There are a usual number of outliers',nbOfOutliers,' out of',nbOfData
       WRITE(outputFileUnit66,*) 'There are a usual number of outliers at 2 s:',nbOfOutliers,' out of',nbOfData
   ENDIF

   WRITE(outputFileUnit66,*) 'Points with value of first column larger than 3 are suspect, if there are more than ', &
                              nbOfData * 0.003 , ' of them'

!     8) Writing second output
!     ------------------------
   CALL openFile(outputFile67)

   outputFileUnit67 = getFileUnit(outputFile67)

   CALL madmed(ptrSValue,ptrIwValue,nbOfData,med,mad)

   DO i1 = nbOfData, 1, -1
      IF ( ptrSValue(i1) >  (3. * mad) ) THEN
        ptrIW = ptrIwValue(i1)
        WRITE(outputFileUnit67,1234) ptrSValue(i1)/mad,ptrIW,xdValue%values(ptrIW),ydValue%values(ptrIW), &
                                     ddValue%values(ptrIW),dadValue%values(ptrIW),edValue%values(ptrIW)
      ENDIF
   ENDDO

   WRITE(outputFileUnit67,*) 'Points with value of first column larger than 3 are suspect, if there are more than ', &
                              nbOfData * 0.003, ' of them'

   PRINT*, 'relative biais in misfits is',med
   WRITE(outputFileUnit66,*) 'relative biais in misfits is',med
   WRITE(outputFileUnit67,*) 'relative biais in misfits is',med
   PRINT*, 'normalized variance should be one but is ',mad
   WRITE(outputFileUnit66,*) 'normalized variance should be one but is',mad
   WRITE(outputFileUnit67,*) 'normalized variance should be one but is',mad

   CALL closeFile(outputFile66)
   CALL closeFile(outputFile67)

!  Always finalise the DIVA context
!  ================================
   CALL vectorDestroy(sValue)
   CALL vectorDestroy(saValue)
   CALL vectorDestroy(ddValue)
   CALL vectorDestroy(dadValue)
   CALL vectorDestroy(edValue)
   CALL vectorDestroy(xdValue)
   CALL vectorDestroy(ydValue)
   CALL vectorDestroy(iwValue)

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

! Procedure 1 : median
! --------------------
SUBROUTINE median(realValue,integerValue,nbOfData,medianValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfData
      REALType, DIMENSION(:), POINTER :: realValue
      INTEGER(KIND=4), DIMENSION(:), POINTER :: integerValue
      REALType, INTENT(OUT) :: medianValue

      INTEGER :: i1, i2

!     Body
!     - - -

      CALL QS2I1R(realValue,integerValue,nbOfData)

      IF ( mod(nbOfData,2) == 0 ) THEN
         i1 = nbOfData / 2
         i2 = i1 + 1
         medianValue = 0.5 * ( realValue(i1) + realValue(i2) )
      ELSE
         i1 = ( nbOfData + 1 ) / 2
         medianValue = realValue(i1)
      ENDIF

END SUBROUTINE

! Procedure 2 : madMed
! --------------------
SUBROUTINE madmed(realValue,integerValue,nbOfData,medianValue,madValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: nbOfData
      REALType, DIMENSION(:), POINTER :: realValue
      INTEGER(KIND=4), DIMENSION(:), POINTER :: integerValue
      REALType, INTENT(INOUT) :: medianValue
      REALType, INTENT(OUT) :: madValue

!     Body
!     - - -
      CALL median(realValue,integerValue,nbOfData,medianValue)

      realValue(1:nbOfData) = abs(realValue(1:nbOfData)-medianValue)

      CALL median(realValue,integerValue,nbOfData,madValue)

      madValue = 1.4826 * madValue

END SUBROUTINE

! Procedure 3 : compute the number of data
! ----------------------------------------
FUNCTION computeNumberOfData(inputFileUnit1,inputFileUnit2,inputFileUnit3) RESULT(nbOfData)


!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit1, inputFileUnit2, inputFileUnit3

      INTEGER :: nbOfData

!     Body
!     - - -

      nbOfData = 0

1     CONTINUE
      READ(inputFileUnit1,*,END=999)
      READ(inputFileUnit2,*,END=999)
      READ(inputFileUnit3,*,END=999)

      nbOfData = nbOfData + 1

      GOTO 1

999   CONTINUE

END FUNCTION

! Procedure 4 : check if data are valid
! --------------------------------------
SUBROUTINE checkCoherence(aValue,bValue,num)

!     Declaration
!     - - - - - -
      REALType, INTENT(IN) :: aValue, bValue
      INTEGER, INTENT(IN) :: num
      REALType, PARAMETER :: eps = 0.00001

!     Body
!     - - -

      IF ( abs(aValue-bValue) > eps * abs(aValue) ) THEN
         PRINT*,'Incoherent files'
         PRINT*,'difference found in record',num
         STOP
      ENDIF


END SUBROUTINE

END PROGRAM lookForOutliers


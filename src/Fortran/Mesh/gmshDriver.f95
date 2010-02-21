PROGRAM gmshDriver

! This program is a driver from diva data to GMSH data file in order
! to make the meshing procedure with GMSH.
!

#define _RELEASE_VERSION_
!#define _WITH_DEBUG_

#ifdef _WITH_DEBUG_
#undef _RELEASE_VERSION_
#endif

! Module to use
! =============
 USE moduleDIVA
 USE moduleFile
 USE vectorInterface
 USE stencilInterface
 USE nodeInterface
 USE contourInterface
 USE lineInterface
 USE tria3Interface
 USE moduleCoordinateInformation, initialiseCoordinateInformation => initialise
 USE moduleSort


 INCLUDE 'iodv.h'

!     Declaration
!     ===========
!        General
!        -------
         INTEGER :: i1, i2, fileUnit
         REAL(KIND=8) :: deltaXInKm, deltaYInKm, Xmin, Xmax, Ymin, Ymax
         REAL(KIND=8) :: changeCoordinate
         REAL(KIND=8), PARAMETER :: one = 1., zero = 0.

         REAL(KIND=8) :: meshCharacteristicLength
         TYPE(file) :: inputGeneralFile

!        Boundary
!        --------
         INTEGER :: nbOfContour, nbOfNodeInBoundaryI1
         INTEGER, TARGET :: boundaryNodeIndex
         INTEGER, POINTER :: boundarySegmentIndex

         TYPE(contourDataBase) :: boundaryLoops
         TYPE(contour), POINTER :: ptrBoundaryLoop

         TYPE(line) :: lineElement
         TYPE(line), POINTER :: ptrLine
         TYPE(lineDataBase), POINTER :: ptrBoundaryLoopSegment

         TYPE(node) :: nodeElement = node(0.,0.,0.,0,0.)
         TYPE(node), TARGET :: nodeElement1 = node(0.,0.,0.,0,0.), nodeElementOld = node(0.,0.,0.,0,0.)
         TYPE(node), POINTER :: ptrNode, ptrNode1

         TYPE(file) :: inputCoastFile

!        GMSH
!        ----
         TYPE(file) :: outputGMSHFile, outputGMSHMeshFile

!        Reload result
!        -------------
         TYPE(file) :: outputDIVAFile, outputDIVAFileValue
#ifdef _WITH_DEBUG_         
         TYPE(file) :: outputGMSHMeshFile2
#endif         
         INTEGER, PARAMETER :: waitingTime = 1, informationLine = 4

         INTEGER :: nbOfVertices, nbOfCell, nbOfEdges, nbOfSortingNode
         TYPE(nodeDataBase) :: vertices
         TYPE(tria3DataBase) :: cells
         TYPE(tria3), POINTER :: ptrCell
         TYPE(vectorInteger4) :: nodeSorting, nodeSorting2
         INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrNodeSorting, ptrNodeSorting2
         TYPE(node), DIMENSION(:), POINTER :: ptrNodeDB
         TYPE(tria3), DIMENSION(:), POINTER :: ptrCellDB

! ==================================================================================
! ==================================================================================
!  Start Diva mesh code
! ==================================================================================
! ==================================================================================
      CALL createDIVAContext()
      CALL initialiseCoordinateInformation()
      CALL setGmshWithCoastRefinement(.TRUE.)

! ==================================================================================
! ==================================================================================
!  General information
! ==================================================================================
! ==================================================================================

!  Defined file name and open the file
!  ====================================
   CALL createFile(inputGeneralFile,'fort.11',formType=STD_FORMATTED)

   IF ( iodv == 1 ) THEN
      CALL defineFileName(inputGeneralFile,'meshgen.prm')
   ENDIF

   CALL openFile(inputGeneralFile)
   fileUnit = getFileUnit(inputGeneralFile)

! Read data
! =========
   READ(fileUnit,*)
   READ(fileUnit,*) meshCharacteristicLength

   CALL closeFile(inputGeneralFile)

! ==================================================================================
! ==================================================================================
!  Read Coast (Boundary)
! ==================================================================================
! ==================================================================================

!  Defined file name and open the file
!  ====================================
   CALL createFile(inputCoastFile,'fort.10',formType=STD_FORMATTED)

   IF ( iodv == 1 ) THEN
      CALL defineFileName(inputCoastFile,'domain.checked')
   ENDIF

   CALL openFile(inputCoastFile)
   fileUnit = getFileUnit(inputCoastFile)

!  Read the boundaries (coast) of the computational domain
!  =======================================================

      Xmin = 1.D9
      Xmax = -1.D9
      Ymin = 1.D9
      Ymax = -1.D9

!     Read the number of boundaries and create the data base
!     ------------------------------------------------------
      READ(fileUnit,*) nbOfContour
      CALL contourDBCreate(boundaryLoops,nbOfContour)

!     General loop to read the boundaries
!     -----------------------------------
      boundaryNodeIndex = 0
      boundarySegmentIndex => boundaryNodeIndex

      DO i1 = 1, nbOfContour

!         Read the number of "potential" element in the boundary loop
!         +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1) ! take a pointer on the i1_th boundary loop in the data base
          READ(fileUnit,*) nbOfNodeInBoundaryI1                  ! read the number of nodes describing the boundary

!         Define "potential" dimension of the data base containing the boundary segments
!         ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ptrBoundaryLoopSegment => ptrBoundaryLoop%lineDB

          CALL lineDBAddSize(ptrBoundaryLoopSegment,nbOfNodeInBoundaryI1) ! reserve memory to store the segments of the boundary

!         Read the segments of the boundary
!         +++++++++++++++++++++++++++++++++
!            Read the first segment
!            - - - - - - - - - - - -
             boundaryNodeIndex = boundaryNodeIndex + 1
             nodeElement1%indexValue = boundaryNodeIndex

             READ(fileUnit,*) nodeElement1%xValue, nodeElement1%yValue
             lineElement%startNode = nodeElement1
             lineElement%indexValue = boundarySegmentIndex

             ptrNode1 => nodeElement1
             ptrNode => nodeElementOld
             ptrNode%xValue = ptrNode1%xValue
             ptrNode%yValue = ptrNode1%yValue

             Xmin = min(Xmin,ptrNode1%xValue)
             Xmax = max(Xmax,ptrNode1%xValue)
             Ymin = min(Ymin,ptrNode1%yValue)
             Ymax = max(Ymax,ptrNode1%yValue)

!            Read the following segments
!            - - - - - - - - - - - - - -
             DO i2 = 2 , nbOfNodeInBoundaryI1 - 1

!                Read the node coordinates
!                + + + + + + + + + + + + + +
                 READ(fileUnit,*) nodeElement%xValue, nodeElement%yValue

!                Check if this node is not identical to the previous one
!                + + + + + + + + + + + + + + + + + + + + + + + + + + + +

                 IF ( checkNewNode(ptrNode,nodeElement) ) THEN

                      boundaryNodeIndex = boundaryNodeIndex + 1
                      nodeElement%indexValue = boundaryNodeIndex

                      lineElement%endNode = nodeElement

                      CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                      lineElement%startNode = nodeElement
                      lineElement%indexValue = boundarySegmentIndex

                      ptrNode%xValue = nodeElement%xValue
                      ptrNode%yValue = nodeElement%yValue

                     Xmin = min(Xmin,ptrNode%xValue)
                     Xmax = max(Xmax,ptrNode%xValue)
                     Ymin = min(Ymin,ptrNode%yValue)
                     Ymax = max(Ymax,ptrNode%yValue)

                 ENDIF

             ENDDO

!            Read the last segment
!            - - - - - - - - - - -
!                Read the node coordinates
!                + + + + + + + + + + + + + +
            READ(fileUnit,*) nodeElement%xValue, nodeElement%yValue

!                Check if this node is not identical to the previous one
!                + + + + + + + + + + + + + + + + + + + + + + + + + + + +
            IF ( checkNewNode(ptrNode,nodeElement) ) THEN

                    Xmin = min(Xmin,nodeElement%xValue)
                    Xmax = max(Xmax,nodeElement%xValue)
                    Ymin = min(Ymin,nodeElement%yValue)
                    Ymax = max(Ymax,nodeElement%yValue)


!                   Check of this node is not identical to the first node
!                   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
                    IF ( checkNewNode(ptrNode1,nodeElement) ) THEN

                         boundaryNodeIndex = boundaryNodeIndex + 1
                         nodeElement%indexValue = boundaryNodeIndex

                         lineElement%endNode = nodeElement

                         CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                         lineElement%startNode = nodeElement
                         lineElement%indexValue = boundarySegmentIndex
                         lineElement%endNode = ptrNode1

                         CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                    ELSE

                         lineElement%endNode = ptrNode1
                         CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                    ENDIF


            ELSE

!                   Check of this node is not identical to the first node
!                   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
                    IF ( checkNewNode(ptrNode1,nodeElement) ) THEN

                         lineElement%endNode = ptrNode1
                         CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                    ELSE
                         ptrLine => lineDBGetPointerOnLastValue(ptrBoundaryLoopSegment)
                         ptrLine%endNode = ptrNode1
                    ENDIF

            ENDIF

      ENDDO

!  Close the file
!  ==============
   CALL closeFile(inputCoastFile)

! ==================================================================================
! ==================================================================================
! Search domain to be meshed
! ==================================================================================
! ==================================================================================

   CALL searchDomainToBeMeshed(boundaryLoops)

! ==================================================================================
! ==================================================================================
! Now implement coordinate change on the N points and Density regions if ncessessary
! ==================================================================================
! ==================================================================================

   READ(5,*,ERR=9911,END=9911) changeCoordinate

   IF ( changeCoordinate > 0. ) THEN
      CALL setIChangeCoordinate(1)
   ENDIF

   IF ( changeCoordinate > 1.5 ) THEN
      PRINT*,'Pseudo-spherical'
      CALL setISpheric(1)
   ENDIF

   IF ( changeCoordinate < 0. ) THEN
      CALL setIChangeCoordinate(-1)
   ENDIF

9911 CONTINUE

    PRINT*,'Coordinates ', getIChangeCoordinate(), getISpheric(), changeCoordinate

    CALL setMinimumLongitude(Xmin)
    CALL setMaximumLongitude(Xmax)
    CALL setMinimumLatitude(Ymin)
    CALL setMaximumLatitude(Ymax)
    
    CALL computeMeanLongitude()
    CALL computeMeanLatitude()

    IF ( getIChangeCoordinate() < 0 ) THEN
      PRINT*,'Anisotropic case'
      CALL setDeltaYInKm(one)
      CALL setDeltaXInKm((-1.)*changeCoordinate)
    ENDIF

    IF ( getIChangeCoordinate() == 1 ) THEN

      deltaYInKm = ( 4. * asin(1.) * 6360. ) / 360.
      deltaXInKm = asin(1.) * getMeanLatitude() / 90.
      deltaXInKm = 6360. * cos( deltaXInKm )
      deltaXInKm = ( 4. * asin(1.) * deltaXInKm ) / 360.

      CALL setDeltaXInKm(deltaXInKm)
      CALL setDeltaYInKm(deltaYInKm)

      IF ( getISpheric() /= 1 ) THEN
         meshCharacteristicLength = meshCharacteristicLength * deltaYInKm
      ENDIF
    ENDIF

    IF ( getISpheric() == 1 ) THEN
      PRINT*,'Spherical case'
      CALL setMeanLatitude(zero)
      CALL setDeltaXInKm(one)
      CALL setDeltaYInKm(one)
      PRINT*,'Mean longitude',getMeanLongitude()
    ENDIF


    IF ( getIChangeCoordinate() /= 0 ) THEN

      DO i1 = 1, nbOfContour

          ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1) ! take a pointer on the i1_th boundary loop in the data base
          ptrBoundaryLoopSegment => ptrBoundaryLoop%lineDB
          CALL changeLongLatToXY(ptrBoundaryLoopSegment)

      ENDDO

    END IF

! ==================================================================================
! ==================================================================================
! Now here, subtract mean coordinate position to enhance numerical
! precision on mesh-generation
! ==================================================================================
! ==================================================================================

    CALL computeMeanCoordinate(boundaryLoops)
    PRINT*, 'Now centering coordinates ',getMeanXCoordinate(), getMeanYCoordinate()

    CALL computeDimensionLessLength()
    PRINT*,'characteristicLength ',getDimensionLessLength()
    meshCharacteristicLength = meshCharacteristicLength / getDimensionLessLength()
    
    CALL shiftAllCoordinate(boundaryLoops)
    CALL shiftMinimumLongitude()
    CALL shiftMinimumLatitude()
    CALL shiftMaximumLongitude()
    CALL shiftMaximumLatitude()


! ==================================================================================
! ==================================================================================
! Export boundary to gmsh and run
! ==================================================================================
! ==================================================================================

     CALL createFile(outputGMSHFile,'diva.geo',formType=STD_FORMATTED)
     CALL createFile(outputGMSHMeshFile,'diva.mesh',formType=STD_FORMATTED)
     CALL exportBoundaryToGMSH(outputGMSHFile,boundaryLoops,meshCharacteristicLength)
     CALL runGMSH(outputGMSHFile,outputGMSHMeshFile)

     CALL contourDBDestroy(boundaryLoops)
     
! ==================================================================================
! ==================================================================================
! read the result
! ==================================================================================
! ==================================================================================

   CALL sleep(waitingTime)

!  Defined file name and open the file
!  ====================================
   CALL openFile(outputGMSHMeshFile)
   fileUnit = getFileUnit(outputGMSHMeshFile)

!  Read the resulting mesh
!  =======================
!     Read information
!     ----------------
      DO i1 = 1, informationLine
          READ(fileUnit,*)
      ENDDO

!     Read vertices
!     -------------

      READ(fileUnit,*) nbOfVertices
      CALL nodeDBCreate(vertices,nbOfVertices)

      DO i1 = 1, nbOfVertices
         ptrNode => vertices%values(i1)
         ptrNode%indexValue = i1
         READ(fileUnit,*) ptrNode%xValue, ptrNode%yValue, ptrNode%zValue
      ENDDO

!     Read contour edges (not used in Diva)
!     ------------------
      READ(fileUnit,*)

      READ(fileUnit,*) nbOfEdges

      DO i1 = 1, nbOfEdges
         READ(fileUnit,*)
      ENDDO

!     Read information
!     ----------------
      READ(fileUnit,*)

      READ(fileUnit,*) nbOfCell
      CALL tria3DBCreate(cells,nbOfCell)
      DO i1 = 1, nbOfCell
          ptrCell => cells%values(i1)
          ptrCell%indexValue = i1
          READ(fileUnit,*) ptrCell%node1,ptrCell%node2,ptrCell%node3,ptrCell%neighbor1,ptrCell%neighbor2,ptrCell%neighbor3
      ENDDO

      READ(fileUnit,*)

   CALL closeFile(outputGMSHMeshFile)
   
! ==================================================================================
! ==================================================================================
! Prepare data for outputs
! ==================================================================================
! ==================================================================================
   nbOfSortingNode = nbOfVertices
   nbOfVertices = nbOfSortingNode - ( nbOfEdges + ( nbOfCell * 3 - nbOfEdges ) / 2 )
   
   CALL vectorCreate(nodeSorting,nbOfSortingNode)
   CALL vectorCreate(nodeSorting2,nbOfSortingNode)
   CALL sortingNode(vertices,cells,nodeSorting,nodeSorting2,nbOfVertices)
   
! ==================================================================================
! ==================================================================================
! Write results for DIVA
! ==================================================================================
! ==================================================================================

!  Defined file name and open the file
!  ====================================
   CALL createFile(outputDIVAFile,'fort.22',formType=STD_FORMATTED)

   IF ( iodv == 1 ) THEN
      CALL defineFileName(outputDIVAFile,'fort.11')
   ENDIF

   CALL openFile(outputDIVAFile)
   fileUnit = getFileUnit(outputDIVAFile)

! Write data
! ==========
   CALL unshiftAllCoordinate(vertices)
   CALL unshiftMinimumLongitude()
   CALL unshiftMinimumLatitude()
   CALL unshiftMaximumLongitude()
   CALL unshiftMaximumLatitude()
   
   IF ( getIChangeCoordinate() /= 0 ) THEN
     CALL changeXYToLongLat(vertices)
   ENDIF

   ptrNodeSorting => vectorGetValues(nodeSorting)
   ptrNodeSorting2 => vectorGetValues(nodeSorting2)
   ptrCellDB => tria3DBGetValues(cells)
   ptrNodeDB => nodeDBGetValues(vertices)

   DO i1 = 1, nbOfVertices
      i2 = ptrNodeSorting2(i1)
      ptrNode => ptrNodeDB(i2)
      WRITE(fileUnit,*) ptrNodeSorting(i1), ptrNode%xValue,ptrNode%yValue
   ENDDO
   
   DO i1 = nbOfVertices + 1, nbOfSortingNode
      WRITE(fileUnit,*) ptrNodeSorting(i1)
   ENDDO
   
   DO i1 = 1, nbOfCell
      ptrCell => ptrCellDB(i1)
      WRITE(fileUnit,*) ptrCell%node1, ptrCell%neighbor1, ptrCell%node2, ptrCell%neighbor2, ptrCell%node3, ptrCell%neighbor3 
   ENDDO

   CALL closeFile(outputDIVAFile)
   
!  Defined file name and open the file
!  ====================================
   CALL createFile(outputDIVAFileValue,'fort.23',formType=STD_FORMATTED)

   CALL openFile(outputDIVAFileValue)
   fileUnit = getFileUnit(outputDIVAFileValue)

   WRITE(fileUnit,*) nbOfVertices
   WRITE(fileUnit,*) nbOfSortingNode - nbOfVertices
   WRITE(fileUnit,*) nbOfCell

   CALL closeFile(outputDIVAFileValue)   
   
   ptrNodeDB(1:nbOfVertices) = ptrNodeDB(ptrNodeSorting2(1:nbOfVertices))
   DO i1 = 1,nbOfVertices
       ptrNode => ptrNodeDB(i1)
       ptrNode%indexValue = i1
   ENDDO
   
#ifdef _RELEASE_VERSION_
   CALL removeUnsedFile(outputGMSHFile,outputGMSHMeshFile)
#else
   CALL createFile(outputGMSHMeshFile2,'test.msh',formType=STD_FORMATTED)
   CALL exportToGMSH(outputGMSHMeshFile2,vertices,cells)
#endif   

   CALL nodeDBDestroy(vertices)
   CALL tria3DBDestroy(cells)
   CALL vectorDestroy(nodeSorting)
   CALL vectorDestroy(nodeSorting2)
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

! Procedure 1 : check if the new node is equal or not to previous one
! -------------------------------------------------------------------
FUNCTION  checkNewNode(ptrNode,newNode) RESULT(check)

!     Declaration
!     - - - - - -
      TYPE(node), POINTER :: ptrNode
      TYPE(node), INTENT(IN) :: newNode

      REAL(KIND=8), PARAMETER :: tolerance = 1.D-6
      REAL(KIND=8) :: xNew, yNew, xOld, yOld
      LOGICAL :: check

!     Body
!     - - -

      check = .TRUE.

      xOld = ptrNode%xValue
      yOld = ptrNode%yValue

      xNew = newNode%xValue
      yNew = newNode%yValue

      IF ( abs( xNew - xOld ) >= tolerance * abs( xNew + xOld ) ) THEN
         RETURN
      ENDIF

      IF ( abs( yNew - yOld ) >= tolerance * abs( yNew + yOld ) ) THEN
         RETURN
      ENDIF

!      PRINT*, 'Found two successive identical points ', ptrNode%indexValue

      check = .FALSE.

END FUNCTION

! Procedure 2 : export data to GMSH
! ---------------------------------
SUBROUTINE exportBoundaryToGMSH(outputGMSHFile,boundaryLoops,meshCharacteristicLength)

!     Declaration
!     - - - - - -
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops
      REAL(KIND=8), INTENT(IN) :: meshCharacteristicLength
      TYPE(file), INTENT(IN) :: outputGMSHFile

      INTEGER :: i1, nbOfContour, nbOfBoundarySegment, fileUnit
      TYPE(contour), POINTER :: ptrBoundaryLoop
      TYPE(lineDataBase), POINTER :: ptrLineDB

!     Body
!     - - -
      CALL openFile(outputGMSHFile)
      fileUnit = getFileUnit(outputGMSHFile)

      nbOfContour = contourDBGetSize(boundaryLoops)

!        Define characteristic length of each node
!        + + + + + + + + + + + + + + + + + + + + +
         DO i1 = 1, nbOfContour
            ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
            ptrLineDB => ptrBoundaryLoop%lineDB
            nbOfBoundarySegment = lineDBGetSize(ptrLineDB)
            ptrLineDB%values(1:nbOfBoundarySegment)%startNode%characteristicLength = meshCharacteristicLength
            ptrLineDB%values(1:nbOfBoundarySegment)%endNode%characteristicLength = meshCharacteristicLength
         ENDDO

!        Export nodes
!        + + + + + + +
         DO i1 = 1, nbOfContour
            ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
            ptrLineDB => ptrBoundaryLoop%lineDB
            CALL exportBoundaryLoopNodesToGMSH(fileUnit,ptrLineDB)
         ENDDO

!        Export boundary segments
!        + + + + + + + + + + + + +
         DO i1 = 1, nbOfContour
            ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
            ptrLineDB => ptrBoundaryLoop%lineDB
            CALL exportBoundaryLoopDataBaseToGMSH(fileUnit,ptrLineDB)
         ENDDO

!        Export boundary loop
!        + + + + + + + + + + +
         DO i1 = 1, nbOfContour
            ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
            CALL exportLoopToGMSH(fileUnit,ptrBoundaryLoop)
         ENDDO

!        Export contour
!        + + + + + + + +
         DO i1 = 1, nbOfContour
            ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
            CALL exportContourToGMSH(fileUnit,ptrBoundaryLoop)
         ENDDO

!        Export command
!        + + + + + + + +
         IF ( getGmshWithCoastRefinement() ) THEN
            CALL exportCommandToGMSHWithRefinement(fileUnit,boundaryLoops,meshCharacteristicLength)
         ELSE
            CALL exportCommandToGMSHWithoutRefinement(fileUnit,boundaryLoops,meshCharacteristicLength)
         ENDIF

      CALL closeFile(outputGMSHFile)

END SUBROUTINE

! Procedure 3 : export nodes to GMSH
! -----------------------------------
SUBROUTINE exportBoundaryLoopNodesToGMSH(fileUnit,ptrLineDB)

!     Declaration
!     - - - - - -
      TYPE(lineDataBase), POINTER :: ptrLineDB
      INTEGER, INTENT(IN) :: fileUnit
      INTEGER :: nbOfBoundarySegment, i1

!     Body
!     - - -
      nbOfBoundarySegment = lineDBGetSize(ptrLineDB)
      DO i1 = 1, nbOfBoundarySegment
           ptrLine => ptrLineDB%values(i1)
           CALL exportNodeToGMSH(fileUnit,ptrLine%startNode)
      ENDDO

END SUBROUTINE

! Procedure 4 : export boundary loop database to GMSH
! ----------------------------------------------------
SUBROUTINE exportBoundaryLoopDataBaseToGMSH(fileUnit,ptrLineDB)

!     Declaration
!     - - - - - -
      TYPE(lineDataBase), POINTER :: ptrLineDB
      INTEGER, INTENT(IN) :: fileUnit
      INTEGER :: nbOfBoundarySegment, i1

!     Body
!     - - -
      nbOfBoundarySegment = lineDBGetSize(ptrLineDB)
      DO i1 = 1, nbOfBoundarySegment
           ptrLine => ptrLineDB%values(i1)
           CALL exportBoundarySegmentToGMSH(fileUnit,ptrLine)
      ENDDO

END SUBROUTINE

! Procedure 5 : export boundary segment to GMSH
! ----------------------------------------------
SUBROUTINE exportBoundarySegmentToGMSH(fileUnit,ptrLine)

!     Declaration
!     - - - - - -
      TYPE(line), POINTER :: ptrLine
      INTEGER, INTENT(IN) :: fileUnit

!     Body
!     - - -
      WRITE(fileUnit,820) ptrLine%indexValue,ptrLine%startNode%indexValue,ptrLine%endNode%indexValue

820  FORMAT("Line(",i20,") = {",i20,",",i20,"};")

END SUBROUTINE

! Procedure 6 : export node to GMSH
! ----------------------------------
SUBROUTINE exportNodeToGMSH(fileUnit,nodeToExport)

!     Declaration
!     - - - - - -
      TYPE(node), INTENT(IN) :: nodeToExport
      INTEGER, INTENT(IN) :: fileUnit

!     Body
!     - - -
      WRITE(fileUnit,810) nodeToExport%indexValue,nodeToExport%xValue,nodeToExport%yValue,nodeToExport%zValue, &
                    nodeToExport%characteristicLength

810  FORMAT("Point(",i20,") = {", f20.10,",", f20.10,",", f20.10,",", f20.10,"};")

END SUBROUTINE

! Procedure 7 : export loop to GMSH
! ----------------------------------
SUBROUTINE exportLoopToGMSH(fileUnit,ptrBoundaryLoop)

!     Declaration
!     - - - - - -
      TYPE(contour), POINTER :: ptrBoundaryLoop
      TYPE(lineDataBase), POINTER :: ptrLineDB
      INTEGER, INTENT(IN) :: fileUnit
      INTEGER :: nbOfBoundarySegment, i1

!     Body
!     - - -
      ptrLineDB => ptrBoundaryLoop%lineDB
      nbOfBoundarySegment = lineDBGetSize(ptrLineDB)

      SELECT CASE (nbOfBoundarySegment)
         CASE (1)
            WRITE(fileUnit,860) ptrBoundaryLoop%indexValue,ptrLineDB%values(1)%indexValue
         CASE (2)
            WRITE(fileUnit,830) ptrBoundaryLoop%indexValue,ptrLineDB%values(1)%indexValue
            WRITE(fileUnit,850) ptrLineDB%values(nbOfBoundarySegment)%indexValue
         CASE DEFAULT
           WRITE(fileUnit,830) ptrBoundaryLoop%indexValue,ptrLineDB%values(1)%indexValue
           DO i1 = 2, nbOfBoundarySegment - 1
                WRITE(fileUnit,840) ptrLineDB%values(i1)%indexValue
           ENDDO
           WRITE(fileUnit,850) ptrLineDB%values(nbOfBoundarySegment)%indexValue
      END SELECT


830  FORMAT("Line Loop(",i20,") = {",i20,",")
840  FORMAT("                      ",i20,",")
850  FORMAT("                      ",i20,"};")
860  FORMAT("Line Loop(",i20,") = {",i20,"};")

END SUBROUTINE

! Procedure 8 : export contour to GMSH
! ----------------------------------
SUBROUTINE exportContourToGMSH(fileUnit,ptrBoundaryLoop)

!     Declaration
!     - - - - - -
      TYPE(contour), POINTER :: ptrBoundaryLoop
      TYPE(vectorInteger4), POINTER :: ptrInsideContour
      INTEGER, INTENT(IN) :: fileUnit
      INTEGER :: nbOfInsideContour, i1

!     Body
!     - - -
      IF ( .NOT.(ptrBoundaryLoop%meshFlag) ) THEN
         RETURN
      ENDIF

      ptrInsideContour => ptrBoundaryLoop%insideContour
      nbOfInsideContour = vectorGetSize(ptrInsideContour)

      SELECT CASE (nbOfInsideContour)
         CASE (0)
             WRITE(fileUnit,860) ptrBoundaryLoop%indexValue,ptrBoundaryLoop%indexValue
         CASE DEFAULT
             WRITE(fileUnit,830) ptrBoundaryLoop%indexValue,ptrBoundaryLoop%indexValue
             DO i1 = 1,nbOfInsideContour-1
                WRITE(fileUnit,840) ptrInsideContour%values(i1)
             ENDDO
             WRITE(fileUnit,850) ptrInsideContour%values(nbOfInsideContour)
      END SELECT

830  FORMAT("Plane Surface(",i20,") = {",i20,",")
840  FORMAT("                          ",i20,",")
850  FORMAT("                          ",i20,"};")
860  FORMAT("Plane Surface(",i20,") = {",i20,"};")

END SUBROUTINE


! Procedure 9 : search computational domain which has to be meshed
! -----------------------------------------------------------------
SUBROUTINE searchDomainToBeMeshed(boundaryLoops)

!     Declaration
!     - - - - - -
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops

      INTEGER :: i1, nbOfContour
!     Body
!     - - -
      nbOfContour = contourDBGetSize(boundaryLoops)

      DO i1 = 1, nbOfContour
         CALL searchInsideDomainOfBoundaryLoop(boundaryLoops,i1)
      ENDDO

      CALL flagDomainToBeMeshed(boundaryLoops)

END SUBROUTINE

! Procedure 10 : search inside domain in a specific boundary loop
! ---------------------------------------------------------------
SUBROUTINE searchInsideDomainOfBoundaryLoop(boundaryLoops,contourNumber)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: contourNumber
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops

      TYPE(contour), POINTER :: ptrBoundaryLoop, ptrBoundaryLoop1
      INTEGER :: i1, i2, nbOfContour
      INTEGER(KIND=4) :: index

!     Body
!     - - -
      ptrBoundaryLoop => contourDBGetValue(boundaryLoops,contourNumber)

      nbOfContour = contourDBGetSize(boundaryLoops)

      i2 = 0
      DO i1 = 1, nbOfContour
         IF ( i1 /= contourNumber ) THEN
          ptrBoundaryLoop1 => contourDBGetValue(boundaryLoops,i1)
          IF ( checkIsInside(ptrBoundaryLoop,ptrBoundaryLoop1) ) THEN
             i2 = i2 + 1
             index = ptrBoundaryLoop1%indexValue
             CALL vectorInsertValue(ptrBoundaryLoop%insideContour,i2,index)
          ENDIF
         ENDIF
      ENDDO

END SUBROUTINE

! Procedure 11 : check if the  boundary loop 1 is inside boundary loop
! --------------------------------------------------------------------
FUNCTION checkIsInside(ptrBoundaryLoop,ptrBoundaryLoop1) RESULT(check)

!     Declaration
!     - - - - - -
      TYPE(contour), POINTER :: ptrBoundaryLoop, ptrBoundaryLoop1

      TYPE(node), POINTER :: ptrNode1

      LOGICAL :: check, checkDomainLeftOrRight

      INTEGER :: nbOfBoundarySegment
      REAL(KIND=8) :: xValueNode1, yValueNode1, xValueNodeStart, yValueNodeStart, xValueNodeEnd, yValueNodeEnd
      REAL(KIND=8) :: distance, distanceBelow, distanceAbove, distanceRight, distanceLeft, slope, xIntersection, yIntersection
      REAL(KIND=8) :: distance1, distance2, d1, d2, d3, d4
      TYPE(lineDataBase), POINTER :: ptrLineDB
      TYPE(line), POINTER :: lineBelow, lineAbove, lineRight, lineLeft, ptrLine

!     Body
!     - - -
      check = .TRUE.

      ptrNode1 => ptrBoundaryLoop1%lineDB%values(1)%startNode

      xValueNode1 = ptrNode1%xValue
      yValueNode1 = ptrNode1%yValue

      ptrLineDB => ptrBoundaryLoop%lineDB

      nbOfBoundarySegment = lineDBGetSize(ptrLineDB)

      distanceBelow = 1.D+9
      distanceAbove = 1.D+9
      distanceRight = 1.D+9
      distanceLeft = 1.D+9

      lineBelow => NULL()
      lineAbove => NULL()
      lineRight => NULL()
      lineLeft => NULL()

      DO i1 = 1, nbOfBoundarySegment
         ptrLine => ptrLineDB%values(i1)

         xValueNodeStart = ptrLine%startNode%xValue
         yValueNodeStart = ptrLine%startNode%yValue

         xValueNodeEnd = ptrLine%endNode%xValue
         yValueNodeEnd = ptrLine%endNode%yValue

         IF ( (xValueNodeStart <= xValueNode1).AND.(xValueNodeEnd >= xValueNode1) &
              .OR. &
              (xValueNodeStart >= xValueNode1).AND.(xValueNodeEnd <= xValueNode1) ) THEN

            slope = ( ( xValueNode1 - xValueNodeStart ) / ( xValueNodeEnd - xValueNodeStart ) )
            yIntersection = yValueNodeStart +  slope * ( yValueNodeEnd - yValueNodeStart )
            distance = abs(yIntersection - yValueNode1)

            IF ( yIntersection > yValueNode1 ) THEN
               IF ( distance <= distanceAbove ) THEN
                  IF ( distance < distanceAbove ) THEN 
                      distanceAbove = distance
                      lineAbove => ptrLine
                  ELSE
                      d1 = lineAbove%startNode%xValue-xValueNode1
                      d2 = lineAbove%startNode%yValue-yValueNode1
                      d3 = lineAbove%endNode%xValue-xValueNode1
                      d4 = lineAbove%endNode%yValue-yValueNode1
                      distance1 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      d1 = ptrLine%startNode%xValue-xValueNode1
                      d2 = ptrLine%startNode%yValue-yValueNode1
                      d3 = ptrLine%endNode%xValue-xValueNode1
                      d4 = ptrLine%endNode%yValue-yValueNode1
                      distance2 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      IF ( distance2 < distance1 ) THEN
                         distanceAbove = distance
                         lineAbove => ptrLine
                      ENDIF
                  ENDIF
               ENDIF
            ELSE
               IF ( distance <= distanceBelow ) THEN
                  IF ( distance < distanceBelow ) THEN
                      distanceBelow = distance
                      lineBelow => ptrLine
                  ELSE
                      d1 = lineBelow%startNode%xValue-xValueNode1
                      d2 = lineBelow%startNode%yValue-yValueNode1
                      d3 = lineBelow%endNode%xValue-xValueNode1
                      d4 = lineBelow%endNode%yValue-yValueNode1
                      distance1 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      d1 = ptrLine%startNode%xValue-xValueNode1
                      d2 = ptrLine%startNode%yValue-yValueNode1
                      d3 = ptrLine%endNode%xValue-xValueNode1
                      d4 = ptrLine%endNode%yValue-yValueNode1
                      distance2 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      IF ( distance2 < distance1 ) THEN
                         distanceBelow = distance
                         lineBelow => ptrLine
                      ENDIF
                  ENDIF
               ENDIF
            ENDIF
            CYCLE
         ENDIF

         IF ( (yValueNodeStart <= yValueNode1).AND.(yValueNodeEnd >= yValueNode1) &
              .OR. &
              (yValueNodeStart >= yValueNode1).AND.(yValueNodeEnd <= yValueNode1) ) THEN

            slope = ( ( yValueNode1 - yValueNodeStart ) / ( yValueNodeEnd - yValueNodeStart ) )
            xIntersection = xValueNodeStart +  slope * ( xValueNodeEnd - xValueNodeStart )
            distance = abs(xIntersection - xValueNode1)

            IF ( xIntersection > xValueNode1 ) THEN
               IF ( distance <= distanceRight ) THEN
                  IF ( distance < distanceRight ) THEN
                      distanceRight = distance
                      lineRight => ptrLine
                  ELSE
                      d1 = lineRight%startNode%xValue-xValueNode1
                      d2 = lineRight%startNode%yValue-yValueNode1
                      d3 = lineRight%endNode%xValue-xValueNode1
                      d4 = lineRight%endNode%yValue-yValueNode1
                      distance1 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      d1 = ptrLine%startNode%xValue-xValueNode1
                      d2 = ptrLine%startNode%yValue-yValueNode1
                      d3 = ptrLine%endNode%xValue-xValueNode1
                      d4 = ptrLine%endNode%yValue-yValueNode1
                      distance2 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      IF ( distance2 < distance1 ) THEN
                         distanceRight = distance
                         lineRight => ptrLine
                      ENDIF
                  ENDIF
               ENDIF
            ELSE
               IF ( distance <= distanceLeft ) THEN
                  IF ( distance < distanceLeft ) THEN
                      distanceLeft = distance
                      lineLeft => ptrLine
                  ELSE
                      d1 = lineLeft%startNode%xValue-xValueNode1
                      d2 = lineLeft%startNode%yValue-yValueNode1
                      d3 = lineLeft%endNode%xValue-xValueNode1
                      d4 = lineLeft%endNode%yValue-yValueNode1
                      distance1 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      d1 = ptrLine%startNode%xValue-xValueNode1
                      d2 = ptrLine%startNode%yValue-yValueNode1
                      d3 = ptrLine%endNode%xValue-xValueNode1
                      d4 = ptrLine%endNode%yValue-yValueNode1
                      distance2 = min(d1*d1+d2*d2,d3*d3+d4*d4)
                      IF ( distance2 < distance1 ) THEN
                         distanceLeft = distance
                         lineLeft => ptrLine
                      ENDIF
                  ENDIF
               ENDIF
               
            ENDIF
            CYCLE
         ENDIF

      ENDDO

      IF (.NOT.((associated(lineBelow)).AND.(associated(lineAbove)).AND.(associated(lineRight)).AND.(associated(lineLeft)))) THEN
         check = .FALSE.
         RETURN
      ENDIF

      CALL checkDomainPosition(ptrBoundaryLoop, checkDomainLeftOrRight)
                   ! if checkDomainLeftOrRight = true => the computational domain is on the left side of the boundary
                   !                           = false => the computational domain is on the rigth side of the boundary


      IF ( checkPosition(lineBelow,ptrNode1,checkDomainLeftOrRight) ) THEN
         check = .FALSE.
         RETURN
      ENDIF
      IF ( checkPosition(lineAbove,ptrNode1,checkDomainLeftOrRight) ) THEN
         check = .FALSE.
         RETURN
      ENDIF
      IF ( checkPosition(lineRight,ptrNode1,checkDomainLeftOrRight) ) THEN
         check = .FALSE.
         RETURN
      ENDIF
      IF ( checkPosition(lineLeft,ptrNode1,checkDomainLeftOrRight) ) THEN
         check = .FALSE.
         RETURN
      ENDIF

END FUNCTION

! Procedure 12 : check if the node is on the right side of the boundary
! ---------------------------------------------------------------------
FUNCTION checkPosition(ptrLine,ptrNode1,contourType) RESULT (check)

!     Declaration
!     - - - - - -
      TYPE(line), POINTER :: ptrLine
      TYPE(node), POINTER :: ptrNode1
      LOGICAL, INTENT(IN) :: contourType

      LOGICAL :: check
      REAL(KIND=8) :: xNodeStart, yNodeStart, xNodeEnd, yNodeEnd, vectorialProduct

!     Body
!     - - -
      check = .FALSE.

      xNodeStart = ptrLine%startNode%xValue
      yNodeStart = ptrLine%startNode%yValue
      xNodeEnd = ptrLine%endNode%xValue
      yNodeEnd = ptrLine%endNode%yValue

      vectorialProduct = (xNodeEnd - xNodeStart) * (ptrNode1%yValue - yNodeStart) - &
                         (yNodeEnd - yNodeStart) * (ptrNode1%xValue - xNodeStart)

      IF ( .NOT.(contourType) ) THEN
         vectorialProduct = (-1.) * vectorialProduct
      ENDIF

      IF ( vectorialProduct < 0. ) THEN
         check = .TRUE.
      ENDIF

END FUNCTION

! Procedure 13 : check if the computational domain is on the left side of the boundary or on the right side
! ---------------------------------------------------------------------------------------------------------
SUBROUTINE checkDomainPosition(ptrBoundaryLoop, check)

! Declaration
! -----------
     TYPE(contour), POINTER :: ptrBoundaryLoop

     LOGICAL, INTENT(OUT) :: check
     REAL(KIND=8) :: surface

! Body
! ----
    surface = contourComputeSurface(ptrBoundaryLoop)

    check = .FALSE.
    IF ( surface > 0. ) THEN
       check = .TRUE.
    ENDIF

END SUBROUTINE


! Procedure 14 : flag to domain which has to be meshed
! ----------------------------------------------------
SUBROUTINE flagDomainToBeMeshed(boundaryLoops)

!     Declaration
!     - - - - - -
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops

      TYPE(contour), POINTER :: ptrBoundaryLoop
      TYPE(vectorInteger4), POINTER :: ptrInsideContour
      INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrValues

      INTEGER :: i1, nbOfContour, nbOfValues
      TYPE(vectorInteger4) :: storageVector
      INTEGER(KIND=4), DIMENSION(:), POINTER :: ptrStorageVector
      INTEGER(KIND=4) :: two = 2
      
!     Body
!     - - -

      nbOfContour = contourDBGetSize(boundaryLoops)
      CALL vectorCreate(storageVector,nbOfContour)
      ptrStorageVector => vectorGetValues(storageVector)
      CALL vectorSetToZero(storageVector)

      DO i1 = 1, nbOfContour
          ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
          ptrInsideContour => ptrBoundaryLoop%insideContour
          ptrValues => vectorGetValues(ptrInsideContour)
          nbOfValues = vectorGetSize(ptrInsideContour)
          
          ptrStorageVector(ptrValues(1:nbOfValues)) = ptrStorageVector(ptrValues(1:nbOfValues)) + 1
          
      ENDDO

      ptrStorageVector = modulo(ptrStorageVector,two)

      DO i1 = 1, nbOfContour
          ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
          IF ( ptrStorageVector(i1) == 0 ) THEN
             ptrBoundaryLoop%meshFlag = .TRUE.
          ELSE
             ptrBoundaryLoop%meshFlag = .FALSE.
          ENDIF
      ENDDO

      CALL vectorDestroy(storageVector)

END SUBROUTINE

! Procedure 15 : write command for meshing properties in gmsh
! -----------------------------------------------------------
SUBROUTINE exportCommandToGMSHWithoutRefinement(fileUnit,boundaryLoops,meshCharacteristicLength)

! Declaration
! -----------
     TYPE(contourDataBase), INTENT(IN) :: boundaryLoops
     REAL(KIND=8), INTENT(IN) :: meshCharacteristicLength
     INTEGER, INTENT(IN) :: fileUnit

     INTEGER :: i1 ,i2, nbOfContour, nbOfBoundarySegment
     TYPE(contour), POINTER :: ptrBoundaryLoop
     TYPE(lineDataBase), POINTER :: ptrLineDB


! Body
! ----
     WRITE(fileUnit,810)

     nbOfContour = contourDBGetSize(boundaryLoops)


     DO i1 = 1, nbOfContour
        ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
        ptrLineDB => ptrBoundaryLoop%lineDB
        nbOfBoundarySegment = lineDBGetSize(ptrLineDB)

        DO i2 = 1, nbOfBoundarySegment
          IF ( (i1 == 1).AND.(i2 ==1) ) THEN
             WRITE(fileUnit,830) ptrLineDB%values(i2)%indexValue
          ELSEIF ((i1 == nbOfContour).AND.(i2 ==nbOfBoundarySegment)) THEN
             WRITE(fileUnit,850) ptrLineDB%values(i2)%indexValue
          ELSE
             WRITE(fileUnit,840) ptrLineDB%values(i2)%indexValue
          ENDIF
        ENDDO
     ENDDO

     WRITE(fileUnit,820) meshCharacteristicLength
     WRITE(fileUnit,860) meshCharacteristicLength,meshCharacteristicLength
     WRITE(fileUnit,960) 2

810  FORMAT("Mesh.CharacteristicLengthExtendFromBoundary = 0;")
830  FORMAT("Field[1] = Attractor;",/,"Field[1].EdgesList = {",i20,",")
840  FORMAT("                      ",i20,",")
850  FORMAT("                      ",i20,"};")
820  FORMAT("Field[2] = Threshold;",/,"Field[2].DistMax = ",f20.10,";",/,"Field[2].DistMin = 0.;",/,"Field[2].IField = 1;")
860  FORMAT("Field[2].LcMax = ",f20.10,";",/,"Field[2].LcMin = ",f20.10,";",/,"Field[2].Sigmoid = 1;")

960  FORMAT("Background Field = ",i20,";")

END SUBROUTINE

! Procedure 16 : write command for meshing properties in gmsh
! -----------------------------------------------------------
SUBROUTINE exportCommandToGMSHWithRefinement(fileUnit,boundaryLoops,meshCharacteristicLength)

! Declaration
! -----------
     TYPE(contourDataBase), INTENT(IN) :: boundaryLoops
     REAL(KIND=8), INTENT(IN) :: meshCharacteristicLength
     INTEGER, INTENT(IN) :: fileUnit

     INTEGER :: i1 ,i2, i3,  nbOfContour, nbOfBoundarySegment, totalNbOfBoundarySegment, iLower, iHigher
     INTEGER(KIND=4) :: index
     TYPE(contour), POINTER :: ptrBoundaryLoop
     TYPE(lineDataBase), POINTER :: ptrLineDB
     TYPE(line), POINTER :: ptrLine

     TYPE(vectorInteger4) :: attractorLengthLowerMeshCharacteristic, attractorLengthHigherMeshCharacteristic
     REAL(KIND=8) :: characteristicLength, xValueStart, yValueStart, xValueEnd, yValueEnd, length

! Body
! ----
     WRITE(fileUnit,810)

     nbOfContour = contourDBGetSize(boundaryLoops)

     totalNbOfBoundarySegment = 0

     DO i1 = 1, nbOfContour
        ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
        ptrLineDB => ptrBoundaryLoop%lineDB
        totalNbOfBoundarySegment = totalNbOfBoundarySegment + lineDBGetSize(ptrLineDB)
     ENDDO

     CALL vectorCreate(attractorLengthLowerMeshCharacteristic,totalNbOfBoundarySegment)
     CALL vectorCreate(attractorLengthHigherMeshCharacteristic,totalNbOfBoundarySegment)

     CALL vectorSetToZero(attractorLengthLowerMeshCharacteristic)
     CALL vectorSetToZero(attractorLengthHigherMeshCharacteristic)

     iLower = 0
     iHigher = 0
     characteristicLength = 0.

     DO i1 = 1, nbOfContour
        ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
        ptrLineDB => ptrBoundaryLoop%lineDB
        nbOfBoundarySegment = lineDBGetSize(ptrLineDB)

        DO i2 = 1, nbOfBoundarySegment
          ptrLine => ptrLineDB%values(i2)
          xValueStart = ptrLine%startNode%xValue
          yValueStart = ptrLine%startNode%yValue
          xValueEnd = ptrLine%endNode%xValue
          yValueEnd = ptrLine%endNode%yValue

          length = sqrt( (xValueStart-xValueEnd) * (xValueStart-xValueEnd)  + (yValueStart-yValueEnd) * (yValueStart-yValueEnd) )

          IF ( length < meshCharacteristicLength ) THEN
              iLower = iLower + 1
              index = ptrLine%indexValue
              CALL vectorFastInsertValue(attractorLengthLowerMeshCharacteristic,iLower,index)
              characteristicLength = characteristicLength + length
          ELSE
              iHigher = iHigher + 1
              index = ptrLine%indexValue
              CALL vectorFastInsertValue(attractorLengthHigherMeshCharacteristic,iHigher,index)
          ENDIF
        ENDDO

     ENDDO

     characteristicLength = characteristicLength / max(iLower,1)

     i2 = -1
     IF ( iLower > 0 ) THEN
        i2 = i2 + 2
        WRITE(fileUnit,830) i2, i2, attractorLengthLowerMeshCharacteristic%values(1)
        DO i1 = 2, iLower - 1
           WRITE(fileUnit,840) attractorLengthLowerMeshCharacteristic%values(i1)
        ENDDO
        WRITE(fileUnit,850) attractorLengthLowerMeshCharacteristic%values(iLower)

        i3 = i2 + 1
        WRITE(fileUnit,820) i3,i3,meshCharacteristicLength,i3,characteristicLength
        WRITE(fileUnit,821) i3,i2
        WRITE(fileUnit,860) i3,meshCharacteristicLength,i3,characteristicLength,i3
     ENDIF
     IF ( iHigher > 0 ) THEN
        i2 = i2 + 2
        WRITE(fileUnit,830) i2, i2, attractorLengthHigherMeshCharacteristic%values(1)
        DO i1 = 2, iHigher - 1
           WRITE(fileUnit,840) attractorLengthHigherMeshCharacteristic%values(i1)
        ENDDO
        WRITE(fileUnit,850) attractorLengthHigherMeshCharacteristic%values(iHigher)

        i3 = i2 + 1
        WRITE(fileUnit,820) i3,i3,meshCharacteristicLength,i3,meshCharacteristicLength
        WRITE(fileUnit,821) i3,i2
        WRITE(fileUnit,860) i3,meshCharacteristicLength,i3,meshCharacteristicLength,i3
     ENDIF

     SELECT CASE (i2)
        CASE (1)
           WRITE(fileUnit,960) 2
        CASE (3)
           WRITE(fileUnit,910) 5
           WRITE(fileUnit,920) 5,2,4
           WRITE(fileUnit,960) 5
     END SELECT

     CALL vectorDestroy(attractorLengthLowerMeshCharacteristic)
     CALL vectorDestroy(attractorLengthHigherMeshCharacteristic)

810  FORMAT("Mesh.CharacteristicLengthExtendFromBoundary = 0;")
830  FORMAT("Field[",i20,"] = Attractor;",/,"Field[",i20,"].EdgesList = {",i20,",")
840  FORMAT("                      ",i20,",")
850  FORMAT("                      ",i20,"};")
820  FORMAT("Field[",i20,"] = Threshold;",/,"Field[",i20,"].DistMax = ",f20.10,";",/,"Field[",i20,"].DistMin = ",f20.10,";")
821  FORMAT("Field[",i20,"].IField = ",i20,";")
860  FORMAT("Field[",i20,"].LcMax = ",f20.10,";",/,"Field[",i20,"].LcMin = ",f20.10,";",/,"Field[",i20,"].Sigmoid = 1;")

910  FORMAT("Field[",i20,"] = Min ;")
920  FORMAT("Field[",i20,"].FieldsList = {",i20,",",i20,"};")
960  FORMAT("Background Field = ",i20,";")

END SUBROUTINE

! Procedure 17 : run gmsh
! -----------------------
SUBROUTINE runGMSH(outputGMSHFile,outputGMSHMeshFile)

! Declaration
! -----------
     TYPE(file), INTENT(IN) :: outputGMSHFile, outputGMSHMeshFile
     CHARACTER(LEN=323) :: command

! Body
! ----
     command = 'gmsh -2 -smooth 10 -algo meshadapt -format mesh -order 2 ' // outputGMSHFile%fileName &
               // ' -o ' // outputGMSHMeshFile%fileName
     
     PRINT*,'GMSH is running ...'
     CALL system(command)

END SUBROUTINE

! Procedure 18 : change coordinate
! --------------------------------
SUBROUTINE changeLongLatToXY(ptrBoundaryLoopSegment)

!     Declaration
!     - - - - - -
      TYPE(lineDataBase), POINTER :: ptrBoundaryLoopSegment

      INTEGER :: i1, nbOfData
      REAL(KIND=8), POINTER :: xValue, yValue
      REAL(KIND=8) :: deltaXInKm, deltaYInKm, meanLongitude, meanLatitude, deltaY, pi

!     Body
!     - - -
      meanLongitude = getMeanLongitude()
      nbOfData = lineDBGetSize(ptrBoundaryLoopSegment)

      SELECT CASE (getISpheric())
         CASE (1)
            pi = getPi() / 180.
            deltaY = 0.001 * ( getMaximumLatitude() - getMinimumLatitude() )

            DO i1 = 1, nbOfData
               ptrLine => ptrBoundaryLoopSegment%values(i1)
               xValue =>  ptrLine%startNode%xValue
               yValue =>  ptrLine%startNode%yValue
               xValue = ( xValue - meanLongitude ) * max(cos(yValue*pi),cos(90.*pi-deltaY*pi))

               xValue =>  ptrLine%endNode%xValue
               yValue =>  ptrLine%endNode%yValue
               xValue = ( xValue - meanLongitude ) * max(cos(yValue*pi),cos(90.*pi-deltaY*pi))
            ENDDO

         CASE DEFAULT
            deltaXInKm = getDeltaXInKm()
            deltaYInKm = getDeltaYInKm()

            meanLatitude = getMeanLatitude()

            DO i1 = 1, nbOfData
               ptrLine => ptrBoundaryLoopSegment%values(i1)
               xValue =>  ptrLine%startNode%xValue
               yValue =>  ptrLine%startNode%yValue
               xValue = ( xValue - meanLongitude ) * deltaXInKm
               yValue = ( yValue - meanLatitude ) * deltaYInKm

               xValue =>  ptrLine%endNode%xValue
               yValue =>  ptrLine%endNode%yValue
               xValue = ( xValue - meanLongitude ) * deltaXInKm
               yValue = ( yValue - meanLatitude ) * deltaYInKm
            ENDDO


      END SELECT

END SUBROUTINE

! Procedure 19 : compute mean value of latitude and longitude
! -----------------------------------------------------------
SUBROUTINE computeMeanCoordinate(boundaryLoops)

!     Declaration
!     - - - - - -
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops
      TYPE(contour), POINTER :: ptrBoundaryLoop

      TYPE(lineDataBase), POINTER :: ptrLineDB

      INTEGER :: i1, nbOfContour, nbOfNode, nbOfBoundarySegment
      REAL(KIND=8) :: meanX, meanY

!     Body
!     - - -
      meanX = 0.
      meanY = 0.
      nbOfNode = 0

      nbOfContour = contourDBGetSize(boundaryLoops)

      DO i1 = 1, nbOfContour
         ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
         ptrLineDB => ptrBoundaryLoop%lineDB
         nbOfBoundarySegment = lineDBGetSize(ptrLineDB)
         nbOfNode = nbOfNode + nbOfBoundarySegment
         meanX = meanX + sum(ptrLineDB%values(1:nbOfBoundarySegment)%startNode%xValue)
         meanY = meanY + sum(ptrLineDB%values(1:nbOfBoundarySegment)%startNode%yValue)
      ENDDO

      meanX = meanX / max(nbOfNode,1)
      meanY = meanY / max(nbOfNode,1)

      CALL setMeanXCoordinate(meanX)
      CALL setMeanYCoordinate(meanY)

END SUBROUTINE

! Procedure 20 : shift all coordinate with respect to meanX and meanY
! -------------------------------------------------------------------
SUBROUTINE shiftAllCoordinate(boundaryLoops)

!     Declaration
!     - - - - - -
      TYPE(contourDataBase), INTENT(INOUT) :: boundaryLoops
      TYPE(contour), POINTER :: ptrBoundaryLoop

      TYPE(lineDataBase), POINTER :: ptrLineDB
      TYPE(line), DIMENSION(:), POINTER :: ptrLines

      INTEGER :: i1, nbOfContour, nbOfBoundarySegment
      REAL(KIND=8) :: meanX, meanY, characteristicLength

!     Body
!     - - -
      meanX = getMeanXCoordinate()
      meanY = getMeanYCoordinate()
      characteristicLength = getDimensionLessLength()

      nbOfContour = contourDBGetSize(boundaryLoops)

      DO i1 = 1, nbOfContour
         ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1)
         ptrLineDB => ptrBoundaryLoop%lineDB
         nbOfBoundarySegment = lineDBGetSize(ptrLineDB)
         ptrLines => lineDBGetValues(ptrLineDB)
         ptrLines(1:nbOfBoundarySegment)%startNode%xValue = ( ptrLines(1:nbOfBoundarySegment)%startNode%xValue - meanX ) / &
                                                            characteristicLength
         ptrLines(1:nbOfBoundarySegment)%startNode%yValue = ( ptrLines(1:nbOfBoundarySegment)%startNode%yValue - meanY ) / &
                                                            characteristicLength
         ptrLines(1:nbOfBoundarySegment)%endNode%xValue = ( ptrLines(1:nbOfBoundarySegment)%endNode%xValue - meanX ) / &
                                                          characteristicLength
         ptrLines(1:nbOfBoundarySegment)%endNode%yValue = ( ptrLines(1:nbOfBoundarySegment)%endNode%yValue - meanY ) / &
                                                          characteristicLength
      ENDDO

END SUBROUTINE


! Procedure 21 : sort nodes
! -------------------------
SUBROUTINE sortingNode(vertices,cells,nodeSorting,nodeSorting2,nbOfVertices)

!     Declaration
!     - - - - - -
      TYPE(nodeDataBase), INTENT(IN) :: vertices
      TYPE(tria3DataBase),INTENT(INOUT) :: cells
      TYPE(vectorInteger4), INTENT(INOUT) :: nodeSorting, nodeSorting2
      INTEGER, INTENT(IN) :: nbOfVertices

      INTEGER :: nbOfDataToSort, nbOfCell, i1
      INTEGER(KIND=4) :: iNode, iIntermediateNode
      INTEGER(KIND=4), PARAMETER :: iOne = 1.
      TYPE(tria3), DIMENSION(:), POINTER :: ptrCellDB
      TYPE(tria3), POINTER :: ptrCell
      INTEGER(KIND=4), DIMENSION(:), POINTER ::  ptrNodeSorting, ptrNodeSorting2
      REAL(KIND=8), DIMENSION(:), POINTER ::  ptrNodeSortingRef
      TYPE(vectorReal8) :: nodeSortingRef
      TYPE(vectorInteger4) :: nodeSorting3
      TYPE(node), DIMENSION(:), POINTER :: ptrNodeDB
      INTEGER, POINTER :: ptrNodeValue
      INTEGER(KIND=4), POINTER :: ptrIntValue

!     Body
!     - - -
      nbOfDataToSort = vectorGetSize(nodeSorting)
      
      CALL vectorCreate(nodeSortingRef,nbOfDataToSort)
      CALL vectorCreate(nodeSorting3,nbOfDataToSort)
      CALL vectorSetToZero(nodeSorting3)
      
      ptrNodeSorting => vectorGetValues(nodeSorting)
      ptrNodeSorting2 => vectorGetValues(nodeSorting3)
      ptrNodeSortingRef => vectorGetValues(nodeSortingRef)
      ptrNodeDB => nodeDBGetValues(vertices)
      
      nbOfCell = tria3DBGetSize(cells)
      ptrCellDB => tria3DBGetValues(cells)

!     1) Introduction of existing nodes in the vector to sort
!     + + + + + + + + + + + + + + + + + + + + + + + + + + + +       
      DO i1 = 1, nbOfDataToSort
         ptrNodeSorting(i1) = i1
         ptrNode => ptrNodeDB(i1)
         ptrNodeSortingRef(i1) = ptrNode%xValue + 1.D-7 * ptrNode%yValue
      ENDDO

!     2) Sort procedure
!     + + + + + + + + +
      CALL QS2I1R(ptrNodeSortingRef,ptrNodeSorting,nbOfDataToSort)

!     3) Reorganisation of the nodes
!     + + + + + + + + + + + + + + + +

      iNode = 0
      iIntermediateNode = nbOfVertices
      
      DO i1 = 1, nbOfCell
         ptrCell => ptrCellDB(i1)
         
         ptrNodeValue => ptrCell%node1
         ptrIntValue => ptrNodeSorting2(ptrNodeValue)
         IF ( ptrIntValue <= 0 ) THEN
            iNode = iNode + iOne
            ptrIntValue = iNode
         ENDIF
         ptrNodeValue = ptrIntValue
                  
         ptrNodeValue => ptrCell%node2
         ptrIntValue => ptrNodeSorting2(ptrNodeValue)
         IF ( ptrIntValue <= 0 ) THEN
            iNode = iNode + iOne
            ptrIntValue = iNode
         ENDIF
         ptrNodeValue = ptrIntValue

         ptrNodeValue => ptrCell%node3
         ptrIntValue => ptrNodeSorting2(ptrNodeValue)
         IF ( ptrIntValue <= 0 ) THEN
            iNode = iNode + iOne
            ptrIntValue = iNode
         ENDIF
         ptrNodeValue = ptrIntValue

         ptrNodeValue => ptrCell%neighbor1
         IF ( ptrNodeValue >= 0 ) THEN
             ptrIntValue => ptrNodeSorting2(ptrNodeValue)
             IF ( ptrIntValue <= 0 ) THEN
                iIntermediateNode = iIntermediateNode + iOne
                ptrIntValue = iIntermediateNode
             ENDIF
             ptrNodeValue = (-1) * ptrIntValue        
         ENDIF 

         ptrNodeValue => ptrCell%neighbor2
         IF ( ptrNodeValue >= 0 ) THEN
             ptrIntValue => ptrNodeSorting2(ptrNodeValue)
             IF ( ptrIntValue <= 0 ) THEN
                iIntermediateNode = iIntermediateNode + iOne
                ptrIntValue = iIntermediateNode
             ENDIF
             ptrNodeValue = (-1) * ptrIntValue        
         ENDIF 

         ptrNodeValue => ptrCell%neighbor3
         IF ( ptrNodeValue >= 0 ) THEN
             ptrIntValue => ptrNodeSorting2(ptrNodeValue)
             IF ( ptrIntValue <= 0 ) THEN
                iIntermediateNode = iIntermediateNode + iOne
                ptrIntValue = iIntermediateNode
             ENDIF
             ptrNodeValue = (-1) * ptrIntValue        
         ENDIF 
      ENDDO
      
      
      ptrNodeSorting => vectorGetValues(nodeSorting2)
      
      DO i1 = 1, nbOfDataToSort
         ptrNodeSorting(ptrNodeSorting2(i1)) = i1
      ENDDO

      ptrNodeSorting => vectorGetValues(nodeSorting)
      ptrNodeSorting(1:nbOfDataToSort)=ptrNodeSorting2(ptrNodeSorting(1:nbOfDataToSort))
     
      CALL vectorDestroy(nodeSortingRef)
      CALL vectorDestroy(nodeSorting3)
            
      PRINT*, 'Finish sorting'
      
END SUBROUTINE

! Procedure 22 : unshift all coordinate with respect to meanX and meanY
! -------------------------------------------------------------------
SUBROUTINE unshiftAllCoordinate(vertices)

!     Declaration
!     - - - - - -
      TYPE(nodeDataBase), INTENT(INOUT) :: vertices

      INTEGER :: nbOfVertice
      REAL(KIND=8) :: meanX, meanY, characteristicLength
      TYPE(node), DIMENSION(:), POINTER :: ptrNodeDB

!     Body
!     - - -
      nbOfVertice = nodeDBgetSize(vertices)
      ptrNodeDB => nodeDBGetValues(vertices)
      meanX = getMeanXCoordinate()
      meanY = getMeanYCoordinate()
      characteristicLength = getDimensionLessLength()
      
      ptrNodeDB(1:nbOfVertice)%xValue = ptrNodeDB(1:nbOfVertice)%xValue * characteristicLength + meanX
      ptrNodeDB(1:nbOfVertice)%yValue = ptrNodeDB(1:nbOfVertice)%yValue * characteristicLength + meanY

END SUBROUTINE

! Procedure 23 : change coordinate
! --------------------------------
SUBROUTINE changeXYToLongLat(vertices)

!     Declaration
!     - - - - - -
      TYPE(nodeDataBase) :: vertices

      INTEGER :: i1, nbOfData
      REAL(KIND=8), POINTER :: xValue, yValue
      REAL(KIND=8) :: deltaXInKm, deltaYInKm, meanLongitude, meanLatitude, deltaY, pi
      REAL(KIND=8), PARAMETER :: cst1 = 180., cst2 = 90., cst3 = 0.001
      TYPE(node), DIMENSION(:), POINTER :: ptrVertices
      TYPE(node), POINTER :: ptrNode

!     Body
!     - - -
      meanLongitude = getMeanLongitude()
      nbOfData = nodeDBGetSize(vertices)
      ptrVertices => nodeDBGetValues(vertices)
      

      SELECT CASE (getISpheric())
         CASE (1)
            pi = getPi() / cst1
            deltaY = cst3 * ( getMaximumLatitude() - getMinimumLatitude() )

            DO i1 = 1, nbOfData
               ptrNode => ptrVertices(i1)
               xValue =>  ptrNode%xValue
               yValue =>  ptrNode%yValue
               
               xValue = xValue / max(cos(yValue*pi),cos(cst2*pi-deltaY*pi)) + meanLongitude
               xValue = max(xValue,(-1.)*cst1)
               xValue = min(xValue,cst1)

            ENDDO

         CASE DEFAULT
            deltaXInKm = getDeltaXInKm()
            deltaYInKm = getDeltaYInKm()

            meanLatitude = getMeanLatitude()

            DO i1 = 1, nbOfData
               ptrNode => ptrVertices(i1)
               xValue =>  ptrNode%xValue
               yValue =>  ptrNode%yValue
               xValue = xValue / deltaXInKm + meanLongitude
               yValue = yValue / deltaYInKm + meanLatitude
            ENDDO

      END SELECT

END SUBROUTINE

! Procedure 24 : export mesh to gmsh for vizualisation
! ----------------------------------------------------
SUBROUTINE exportToGMSH(fileToWrite,vertices,cells)

!     Declaration
!     - - - - - -
      TYPE(nodeDataBase), INTENT(IN) :: vertices
      TYPE(tria3DataBase),INTENT(IN) :: cells
      TYPE(file), INTENT(IN) :: fileToWrite

      INTEGER :: fileUnit, nbOfVertices, iStartValue, iEndValue, nbOfCell
      TYPE(node), DIMENSION(:), POINTER :: ptrNodeValue
      TYPE(node), POINTER :: ptrNode

      TYPE(tria3), DIMENSION(:), POINTER :: ptrCellValue
      TYPE(tria3), POINTER :: ptrCell

!     Body
!     - - -
     CALL openFile(fileToWrite)
     fileUnit = getFileUnit(fileToWrite)

     WRITE(fileUnit,810)

!    1) write information
!    - - - - - - - - - - -
!       1.1) nodes
!       + + + + + +
     nbOfVertices = nodeDBGetSize(vertices)
     iStartValue = nodeDBGetFirstIndex(vertices)
     iEndValue = nodeDBGetLastIndex(vertices)
     ptrNodeValue => nodeDBGetValues(vertices)

     WRITE(fileUnit,820)
     WRITE(fileUnit,*) nbOfVertices
     DO i1 = iStartValue, iEndValue
        ptrNode => ptrNodeValue(i1)
        WRITE(fileUnit,*) ptrNode%indexValue, ptrNode%xValue, ptrNode%yValue, ptrNode%zValue
     ENDDO
     WRITE(fileUnit,830)

!       1.2) cell topology
!       + + + + + + + + + +
     nbOfCell = tria3DBGetSize(cells)
     iStartValue = tria3DBGetFirstIndex(cells)
     iEndValue = tria3DBGetLastIndex(cells)
     ptrCellValue => tria3DBGetValues(cells)

     WRITE(fileUnit,840)
     WRITE(fileUnit,*) nbOfCell
     DO i1 = 1, nbOfCell
        ptrCell => ptrCellValue(i1)
        WRITE(fileUnit,*) ptrCell%indexValue,2,2,99,2,ptrCell%node1, ptrCell%node2, ptrCell%node3
     ENDDO
     WRITE(fileUnit,850)

     CALL closeFile(fileToWrite)

810  FORMAT("$MeshFormat",/,"2.1 0 8",/,"$EndMeshFormat")
820  FORMAT("$Nodes")
830  FORMAT("$EndNodes")
840  FORMAT("$Elements")
850  FORMAT("$EndElements")

END SUBROUTINE

! Procedure 25 : remove temporary gmsh files
! ------------------------------------------
SUBROUTINE removeUnsedFile(file1,file2)

!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN) :: file1, file2
      CHARACTER(LEN=323) :: command
      
!     Body
!     - - -
      command = 'rm -f '// file1%fileName
      CALL system(command)       
      command = 'rm -f '// file2%fileName
      CALL system(command)       

END SUBROUTINE

END PROGRAM gmshDriver

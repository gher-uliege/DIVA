PROGRAM inverseCont

! Module to use
! =============
 USE moduleDIVA
 USE moduleFile
 USE vectorInterface
 USE nodeInterface
 USE contourInterface
 USE lineInterface


!     Declaration
!     ===========
!        General
!        -------
         INTEGER :: i1, i2, fileUnit

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

         TYPE(file) :: inputCoastFile, outputCoastFile

         TYPE(vectorInteger4) :: nodeSorting

! ==================================================================================
! ==================================================================================
!  Start Diva mesh code
! ==================================================================================
! ==================================================================================
      CALL createDIVAContext()

! ==================================================================================
! ==================================================================================
!  Read Coast (Boundary)
! ==================================================================================
! ==================================================================================

!  Defined file name and open the file
!  ====================================
   CALL createFile(inputCoastFile,'fort.10',formType=STD_FORMATTED)

   CALL openFile(inputCoastFile)
   fileUnit = getFileUnit(inputCoastFile)

!  Read the boundaries (coast) of the computational domain
!  =======================================================

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

!            Read the following segments
!            - - - - - - - - - - - - - -
             DO i2 = 2 , nbOfNodeInBoundaryI1 - 1

!                Read the node coordinates
!                + + + + + + + + + + + + + +
                 READ(fileUnit,*) nodeElement%xValue, nodeElement%yValue

!                Check if this node is not identical to the previous one
!                + + + + + + + + + + + + + + + + + + + + + + + + + + + +

                 boundaryNodeIndex = boundaryNodeIndex + 1
                 nodeElement%indexValue = boundaryNodeIndex

                 lineElement%endNode = nodeElement

                 CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

                 lineElement%startNode = nodeElement
                 lineElement%indexValue = boundarySegmentIndex

                 ptrNode%xValue = nodeElement%xValue
                 ptrNode%yValue = nodeElement%yValue

             ENDDO

!            Read the last segment
!            - - - - - - - - - - -
!                Read the node coordinates
!                + + + + + + + + + + + + + +
            READ(fileUnit,*) nodeElement%xValue, nodeElement%yValue

            boundaryNodeIndex = boundaryNodeIndex + 1
            nodeElement%indexValue = boundaryNodeIndex

            lineElement%endNode = nodeElement

            CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

            lineElement%startNode = nodeElement
            lineElement%indexValue = boundarySegmentIndex
            lineElement%endNode = ptrNode1

            CALL lineDBFastPushBack(ptrBoundaryLoopSegment,lineElement) ! introduction of the segment in the database

      ENDDO

!  Close the file
!  ==============
   CALL closeFile(inputCoastFile)
   
   
! ==================================================================================
! ==================================================================================
!  Write Coast the inverse way
! ==================================================================================
! ==================================================================================

!  Defined file name and open the file
!  ====================================
   CALL createFile(outputCoastFile,'coastinv.cont',formType=STD_FORMATTED)

   CALL openFile(outputCoastFile)
   fileUnit = getFileUnit(outputCoastFile)
   

!  Write the boundaries
!  ====================
   WRITE(fileUnit,*) nbOfContour
   
      DO i1 = 1, nbOfContour

        ptrBoundaryLoop => contourDBGetValue(boundaryLoops,i1) ! take a pointer on the i1_th boundary loop in the data base
        ptrBoundaryLoopSegment => ptrBoundaryLoop%lineDB
        
        nbOfNodeInBoundaryI1 = lineDBGetSize(ptrBoundaryLoopSegment) 
        
        WRITE(fileUnit,*) nbOfNodeInBoundaryI1
        
        DO i2 = nbOfNodeInBoundaryI1, 1, -1
           ptrLine => lineDBGetValue(ptrBoundaryLoopSegment,i2)
           WRITE(fileUnit,*) ptrLine%startNode%xValue, ptrLine%startNode%yValue
        ENDDO
        
      ENDDO         
        
   CALL closeFile(outputCoastFile)

   CALL contourDBDestroy(boundaryLoops)
   
! If we don't put this, the library libVector is not used so problems   
   CALL vectorCreate(nodeSorting)
   CALL vectorDestroy(nodeSorting)
!
   
   CALL finaliseDIVAContext()


END PROGRAM inverseCont

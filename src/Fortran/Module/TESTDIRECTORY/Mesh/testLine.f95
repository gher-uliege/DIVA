PROGRAM testLine

! =====================================================
! =====================================================
! ===                                               ===
! ===   This program is testing the mesh module     ===
! ===                                               ===
! =====================================================
! =====================================================

! Module to use
! =============
 USE moduleDIVA
 USE nodeInterface
 USE lineInterface

! Declaration
! ===========
TYPE(nodeDataBase) :: nodeDB
TYPE(lineDataBase) :: lineDB

TYPE(node), POINTER :: noeud
TYPE(line), POINTER :: ligne

INTEGER, PARAMETER :: dimNode = 3
INTEGER, PARAMETER :: dimLine = 3

! Main program
! ============
    CALL createDIVAContext()

  CALL nodeDBCreate(nodeDB,dimNode)
  CALL lineDBCreate(lineDB,dimLine)

  CALL nodeDBInitialise(nodeDB)
  CALL lineDBInitialise(lineDB)


  noeud => nodeDBGetValue(nodeDB,1)
  noeud%xValue = 0.
  noeud%yValue = 0.
  noeud%characteristicLength = 0.5

  noeud => nodeDBGetValue(nodeDB,2)
  noeud%xValue = 1.
  noeud%yValue = 0.
  noeud%characteristicLength = 0.5

  noeud => nodeDBGetValue(nodeDB,3)
  noeud%xValue = 0.5
  noeud%yValue = 1.
  noeud%characteristicLength = 0.5

  ligne => lineDBGetValue(lineDB,1)
  ligne%startNode = nodeDBGetValue(nodeDB,1)
  ligne%endNode = nodeDBGetValue(nodeDB,2)

  ligne => lineDBGetValue(lineDB,2)
  ligne%startNode = nodeDBGetValue(nodeDB,2)
  ligne%endNode = nodeDBGetValue(nodeDB,3)

  ligne => lineDBGetValue(lineDB,3)
  ligne%startNode = nodeDBGetValue(nodeDB,3)
  ligne%endNode = nodeDBGetValue(nodeDB,1)


  CALL nodeDBPrint(nodeDB)
  CALL lineDBPrint(lineDB)

  CALL nodeDBDestroy(nodeDB)
  CALL lineDBDestroy(lineDB)

! End program
! ===========
  CALL finaliseDIVAContext()


END PROGRAM testLine


PROGRAM testNode

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

! Declaration
! ===========
TYPE(nodeDataBase) :: nodeDB

INTEGER, PARAMETER :: dim = 3

! Main program
! ============
    CALL createDIVAContext()

!   1) testing the creation procedure
!   ---------------------------------

  PRINT*,'checkAllocationMemoryProcedure'
  PRINT*,'=============================='
  CALL checkAllocationMemoryProcedure()
  PRINT*,' '

!    1.1) Basic creation procedure
!    - - - - - - - - - - - - - - -
  CALL nodeDBCreate(nodeDB)
  CALL nodeDBSetSize(nodeDB,dim)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

!    1.2) creation procedure with dimension
!    - - - - - - - - - - - - - - - - - - - -
  CALL nodeDBCreate(nodeDB,dim)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

!    1.3) creation procedure with dimension and starting point
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL nodeDBCreate(nodeDB,dim,-1)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

!    1.4) Basic creation procedure with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - -

  CALL nodeDBCreate(nodeDB)
  CALL nodeDBSetSize(nodeDB,dim)
  CALL nodeDBOptimizeMemory(nodeDB)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

!    1.5) creation procedure with dimension with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL nodeDBCreate(nodeDB,dim)
  CALL nodeDBOptimizeMemory(nodeDB)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

!    1.6) creation procedure with dimension and starting point with optimal size
!    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CALL nodeDBCreate(nodeDB,dim,-1)
  CALL nodeDBOptimizeMemory(nodeDB)
  CALL nodeDBSetIncreaseSize(nodeDB,0)
  CALL checkProcedure(nodeDB)
  CALL nodeDBDestroy(nodeDB)

! End program
! ===========
    CALL finaliseDIVAContext()

 CONTAINS

! ==============================================
! ==============================================
! ==                                          ==
! ==                                          ==
! ==            Internal procedure            ==
! ==                                          ==
! ==                                          ==
! ==============================================
! ==============================================
 SUBROUTINE checkProcedure(nodeDB)

!  Declaration
!  -----------
   TYPE(nodeDataBase), INTENT(INOUT) :: nodeDB

!  Body
!  - - -
  PRINT*,'checkInitialise'
  PRINT*,'==============='
  CALL checkInitialise(nodeDB)
  PRINT*,' '
  PRINT*,'checkSetInsertValue'
  PRINT*,'==================='
  CALL checkInsertValue(nodeDB)
  PRINT*,' '
  PRINT*,'checkGetValues'
  PRINT*,'=============='
  CALL checkGetValues(nodeDB)
  PRINT*,' '
  PRINT*,'checkSetSize'
  PRINT*,'============'
  CALL checkSetSize(nodeDB)
  PRINT*,' '

 END SUBROUTINE

 SUBROUTINE checkInitialise(nodeDB)

!  Declaration
!  -----------
   TYPE(nodeDataBase), INTENT(INOUT) :: nodeDB

!  Body
!  - - -
   CALL nodeDBInitialise(nodeDB)
   CALL nodeDBPrint(nodeDB)

 END SUBROUTINE

 SUBROUTINE checkInsertValue(nodeDB)

!  Declaration
!  -----------
   TYPE(nodeDataBase), INTENT(INOUT) :: nodeDB
   TYPE(node) :: noeud1=node(1.5,2.5,3.5,-56,0.2)

!  Body
!  - - -
   CALL nodeDBInsert(nodeDB,1,noeud1)
   CALL nodeDBPrint(nodeDB)

 END SUBROUTINE


 SUBROUTINE checkGetValues(nodeDB)

!  Declaration
!  -----------
   TYPE(nodeDataBase), INTENT(INOUT) :: nodeDB
   TYPE(node), DIMENSION(:), POINTER :: ptr
   INTEGER :: istart, iend, i1

!  Body
!  - - -
  ptr => nodeDBGetValues(nodeDB)
  istart = nodeDBGetFirstIndex(nodeDB)
  iend = nodeDBGetLastIndex(nodeDB)
  DO i1 = istart, iend
     ptr(i1)%zValue = 23.
  ENDDO

  CALL nodeDBPrint(nodeDB)

 END SUBROUTINE

 SUBROUTINE checkSetSize(nodeDB)

!  Declaration
!  -----------
   TYPE(nodeDataBase), INTENT(INOUT) :: nodeDB
   INTEGER :: istart, iend, i1
   TYPE(node) :: val = node(1.5,2.5,3.5,-56,0.2)
   TYPE(node) :: val2 = node(1.5,2.5,3.5,16,0.2)
   TYPE(node) :: val3 = node(1.5,2.5,3.5,30,0.2)

!  Body
!  - - -
  istart = nodeDBGetFirstIndex(nodeDB)
  iend = nodeDBGetLastIndex(nodeDB)

   DO i1 = istart, iend
       CALL nodeDBSetValue(nodeDB,i1,val,SET_VALUE)
   ENDDO

   CALL nodeDBPrint(nodeDB)

   DO i1 = istart - 2 , istart - 1
       CALL nodeDBSetValue(nodeDB,i1,val2,INSERT_VALUE)
   ENDDO

   DO i1 = iend + 1  , iend + 2
       CALL nodeDBSetValue(nodeDB,i1,val3,INSERT_VALUE)
   ENDDO

   CALL nodeDBPrint(nodeDB)

 END SUBROUTINE

 SUBROUTINE checkAllocationMemoryProcedure()

!  Declaration
!  -----------
   TYPE(nodeDataBase) :: nodeDB

!  Body
!  - - -
   CALL nodeDBCreate(nodeDB)
   CALL nodeDBSetSize(nodeDB,3)
   CALL nodeDBInitialise(nodeDB)
   CALL nodeDBPrint(nodeDB)
   CALL nodeDBSetIncreaseSize(nodeDB,0)
   CALL nodeDBSetSize(nodeDB,9)
   CALL nodeDBInitialise(nodeDB)
   CALL nodeDBPrint(nodeDB)
   CALL nodeDBSetIncreaseSize(nodeDB,-1)
   CALL nodeDBSetSize(nodeDB,10)
   CALL nodeDBInitialise(nodeDB)
   CALL nodeDBPrint(nodeDB)
   CALL nodeDBDestroy(nodeDB)

 END SUBROUTINE

END PROGRAM testNode


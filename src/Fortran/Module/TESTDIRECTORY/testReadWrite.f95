PROGRAM testUWritc

! Module
! ======
  USE moduleDIVA
  USE moduleFile
  USE moduleReadWrite

! Declaration
! ===========
   INTEGER, PARAMETER :: nbOfDataI = 2
   INTEGER, PARAMETER :: nbOfDataJ = 2
   INTEGER, PARAMETER :: nbOfDataK = 2
   INTEGER, PARAMETER :: nbOfDataDegenerated = 4

   INTEGER :: nbOfWord
   REAL*4 :: exclusionValueReal4
   REAL*8 :: exclusionValueReal8
   
   REAL*4, DIMENSION(nbOfDataI,nbOfDataJ,nbOfDataK) :: real4Matrix
   REAL*4, DIMENSION(nbOfDataDegenerated) :: real4Vector
   REAL*8, DIMENSION(nbOfDataI,nbOfDataJ,nbOfDataK) :: real8Matrix
   REAL*8, DIMENSION(nbOfDataDegenerated) :: real8Vector
   
   TYPE(file) :: outputFile4, outputFile8

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
   exclusionValueReal4 = 999.
   exclusionValueReal8 = 9.99D+2

   real4Matrix(1,1,1)=1.
   real4Matrix(1,2,1)=2.
   real4Matrix(2,1,1)=3.
   real4Matrix(2,2,1)=4.

   real4Matrix(1,1,2)=5.
   real4Matrix(1,2,2)=6.
   real4Matrix(2,1,2)=7.
   real4Matrix(2,2,2)=999.

   real8Matrix(1,1,1)=1.d+0
   real8Matrix(1,2,1)=2.d+0
   real8Matrix(2,1,1)=3.d+0
   real8Matrix(2,2,1)=4.d+0

   real8Matrix(1,1,2)=5.d+0
   real8Matrix(1,2,2)=6.d+0
   real8Matrix(2,1,2)=7.d+0
   real8Matrix(2,2,2)=8.d+0

   real4Vector(1)=0.2
   real4Vector(2)=0.6
   real4Vector(3)=0.1
   real4Vector(4)=0.9
   
   real8Vector(1)=0.2d+0
   real8Vector(2)=0.6d+0
   real8Vector(3)=0.1d+0
   real8Vector(4)=0.9d+0

   CALL createFile(outputFile4,'fort.412',getLogicalUnit(),formType=Formatted())
   CALL createFile(outputFile8,'fort.812',getLogicalUnit(),formType=Formatted())

   nbOfWord=nbOfDataI*nbOfDataJ*nbOfDataK
   CALL uwritc(outputFile4,real4Matrix,exclusionValueReal4,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL uwritc(outputFile8,real8Matrix,exclusionValueReal8,nbOfDataI,nbOfDataJ,nbOfDataK)

   CALL defineFileName(outputFile4,'fort.413')
   CALL defineFileName(outputFile8,'fort.813')
   nbOfWord=iminusone
   CALL uwritc(outputFile4,real4Matrix,exclusionValueReal4,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL uwritc(outputFile8,real8Matrix,exclusionValueReal8,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)

   CALL defineFileName(outputFile4,'fort.414')
   CALL defineFileName(outputFile8,'fort.814')
   nbOfWord=nbOfDataI*nbOfDataJ*nbOfDataK-itwo
   CALL uwritc(outputFile4,real4Matrix,exclusionValueReal4,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL uwritc(outputFile8,real8Matrix,exclusionValueReal8,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)

   CALL defineFileName(outputFile4,'fort.415')
   CALL defineFileName(outputFile8,'fort.815')
   CALL uwritc(outputFile4,real4Vector,exclusionValueReal4,iminusone*nbOfDataI,nbOfWord)
   CALL uwritc(outputFile8,real8Vector,exclusionValueReal8,nbOfDataI)

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM testUWritc

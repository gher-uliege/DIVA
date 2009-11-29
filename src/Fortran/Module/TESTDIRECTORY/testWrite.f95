PROGRAM testUWritc

! Module
! ======
  USE moduleDIVA
  USE ioInterface

! Declaration
! ===========
   INTEGER, PARAMETER :: nbOfDataI = 2
   INTEGER, PARAMETER :: nbOfDataJ = 2
   INTEGER, PARAMETER :: nbOfDataK = 2
   INTEGER, PARAMETER :: nbOfDataDegenerated = 4

   INTEGER :: nbOfWord
   REAL(KIND=4) :: exclusionValue

   REAL(KIND=4), DIMENSION(1:nbOfDataDegenerated) :: real4Vector
   REAL(KIND=4), DIMENSION(1:nbOfDataI,1:nbOfDataJ) :: real4Matrix
   REAL(KIND=4), DIMENSION(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) :: real4Array

   REAL(KIND=8), DIMENSION(1:nbOfDataDegenerated) :: real8Vector
   REAL(KIND=8), DIMENSION(1:nbOfDataI,1:nbOfDataJ) :: real8Matrix
   REAL(KIND=8), DIMENSION(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) :: real8Array

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
   exclusionValue = 999.

   real4Vector(1)=0.2
   real4Vector(2)=0.6
   real4Vector(3)=0.1
   real4Vector(4)=0.9

   real8Vector(1)=0.2d+0
   real8Vector(2)=0.6d+0
   real8Vector(3)=0.1d+0
   real8Vector(4)=0.9d+0

   real4Matrix(1,1)=1.
   real4Matrix(1,2)=2.
   real4Matrix(2,1)=3.
   real4Matrix(2,2)=4.

   real8Matrix(1,1)=1.d+0
   real8Matrix(1,2)=2.d+0
   real8Matrix(2,1)=3.d+0
   real8Matrix(2,2)=4.d+0

   real4Array(1,1,1)=1.
   real4Array(1,2,1)=2.
   real4Array(2,1,1)=3.
   real4Array(2,2,1)=4.

   real4Array(1,1,2)=5.
   real4Array(1,2,2)=6.
   real4Array(2,1,2)=7.
   real4Array(2,2,2)=999.

   real8Array(1,1,1)=1.d+0
   real8Array(1,2,1)=2.d+0
   real8Array(2,1,1)=3.d+0
   real8Array(2,2,1)=4.d+0

   real8Array(1,1,2)=5.d+0
   real8Array(1,2,2)=6.d+0
   real8Array(2,1,2)=7.d+0
   real8Array(2,2,2)=8.d+0


!  Test for real(kind=4)
!  ---------------------
   CALL createFile(outputFile4,'fort.412',formType=GHER_FORMATTED)

!  1) vector
!  ---------
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)

   CALL defineFileName(outputFile4,'fort.413')
   nbOfWord=-1
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)

   CALL defineFileName(outputFile4,'fort.414')
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)

   CALL defineFileName(outputFile4,'fort.415')
   nbOfWord=-1
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)

   CALL defineFileName(outputFile4,'fort.416')
   nbOfWord=nbOfDataDegenerated-ione
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)

   CALL defineFileName(outputFile4,'fort.417')
   nbOfWord=nbOfDataDegenerated-ione
   CALL writeOnDisk(outputFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)

!  2) matrix
!  ---------
   CALL defineFileName(outputFile4,'fort.422')
   nbOfWord=nbOfDataI*nbOfDataJ
   CALL writeOnDisk(outputFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)

   CALL defineFileName(outputFile4,'fort.423')
   nbOfWord=-1
   CALL writeOnDisk(outputFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)

   CALL defineFileName(outputFile4,'fort.424')
   nbOfWord=nbOfDataI*nbOfDataJ-itwo
   CALL writeOnDisk(outputFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)

!  3) array
!  ---------
   CALL defineFileName(outputFile4,'fort.432')
   nbOfWord=nbOfDataI*nbOfDataJ*nbOfDataK
   CALL writeOnDisk(outputFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)

   CALL defineFileName(outputFile4,'fort.433')
   nbOfWord=-1
   CALL writeOnDisk(outputFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)

   CALL defineFileName(outputFile4,'fort.434')
   nbOfWord=nbOfDataI*nbOfDataJ*nbOfDataK-itwo
   CALL writeOnDisk(outputFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)

!  Test for real(kind=8)
!  ---------------------
   CALL createFile(outputFile8,'fort.812',formType=GHER_FORMATTED)

!  1) vector
!  ---------
   CALL writeOnDisk(outputFile8,real8Vector,exclusionValue,nbOfDataDegenerated)

   CALL defineFileName(outputFile8,'fort.814')
   CALL writeOnDisk(outputFile8,real8Vector,exclusionValue,iminusone*nbOfDataDegenerated)

   CALL defineFileName(outputFile8,'fort.816')
   nbOfWord=nbOfDataDegenerated-ione
   CALL writeOnDisk(outputFile8,real8Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)

!  2) matrix
!  ---------
   CALL defineFileName(outputFile8,'fort.822')
   CALL writeOnDisk(outputFile8,real8Matrix,exclusionValue,nbOfDataI,nbOfDataJ)

   CALL defineFileName(outputFile8,'fort.823')
   nbOfWord=nbOfDataI*nbOfDataJ-itwo
   CALL writeOnDisk(outputFile8,real8Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)

!  3) array
!  ---------
   CALL defineFileName(outputFile8,'fort.832')
   CALL writeOnDisk(outputFile8,real8Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)

   CALL defineFileName(outputFile8,'fort.833')
   nbOfWord=nbOfDataI*nbOfDataJ*nbOfDataK-itwo
   CALL writeOnDisk(outputFile8,real8Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)


! Test compatibility with old version
! -----------------------------------
   OPEN(unit=15,file='fort.old.412',form='unformatted')
   CALL uwritc(15,real8Vector,real4Vector,exclusionValue,4,4,1,1,nbOfWord)
   CLOSE(15)
   OPEN(unit=15,file='fort.old.812',form='unformatted')
   CALL uwritc(15,real8Vector,real4Vector,exclusionValue,4,4,1,1,nbOfWord)
   CLOSE(15)


!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM testUWritc

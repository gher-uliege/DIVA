PROGRAM testUReadc

! Module
! ======
  USE moduleDIVA
  USE ioInterface

! Declaration
! ===========

   INTEGER, PARAMETER :: defautlDim = 20
   INTEGER, PARAMETER :: nbOfDataI = 2
   INTEGER, PARAMETER :: nbOfDataJ = 2
   INTEGER, PARAMETER :: nbOfDataK = 2
   INTEGER, PARAMETER :: nbOfDataDegenerated = 4

   INTEGER :: nbOfWord, readDimI, readDimJ, readDimK, iprecision
   REAL(KIND=4) :: exclusionValue, readExclusionValue

   REAL(KIND=4), DIMENSION(1:nbOfDataDegenerated) :: real4Vector
   REAL(KIND=4), DIMENSION(1:nbOfDataI,1:nbOfDataJ) :: real4Matrix
   REAL(KIND=4), DIMENSION(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) :: real4Array
   REAL(KIND=4), DIMENSION(1:defautlDim) :: vector4

   REAL(KIND=4), DIMENSION(:), POINTER :: ptrVectorReal4
   REAL(KIND=4), DIMENSION(:,:), POINTER :: ptrMatrixReal4
   REAL(KIND=4), DIMENSION(:,:,:), POINTER :: ptrArrayReal4

   REAL(KIND=8), DIMENSION(:), POINTER :: ptrVectorReal8
   REAL(KIND=8), DIMENSION(:,:), POINTER :: ptrMatrixReal8
   REAL(KIND=8), DIMENSION(:,:,:), POINTER :: ptrArrayReal8
   REAL(KIND=8), DIMENSION(1:defautlDim) :: vector8

   REAL(KIND=8), DIMENSION(1:nbOfDataDegenerated) :: real8Vector
   REAL(KIND=8), DIMENSION(1:nbOfDataI,1:nbOfDataJ) :: real8Matrix
   REAL(KIND=8), DIMENSION(1:nbOfDataI,1:nbOfDataJ,1:nbOfDataK) :: real8Array

   TYPE(file) :: outputFile4, outputFile8
   TYPE(file) :: ioFile4, ioFile8

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
   CALL createFile(outputFile4,'fort.4120',formType=GHER_FORMATTED)
   CALL createFile(ioFile4,'temp.real4',formType=GHER_UNFORMATTED)

!  1) vector
!  ---------
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile4,ptrVectorReal4,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile4,ptrVectorReal4,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal4)

   CALL defineFileName(outputFile4,'fort.4130')
   nbOfWord=iminusone*nbOfDataDegenerated
   CALL writeOnDisk(ioFile4,real4Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile4,ptrVectorReal4,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile4,ptrVectorReal4,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal4)

!  2) matrix
!  ---------

   CALL defineFileName(outputFile4,'fort.4220')
   nbOfWord=-1
   CALL writeOnDisk(ioFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal4)

   CALL defineFileName(outputFile4,'fort.4230')
   nbOfWord=iminusone
   CALL writeOnDisk(ioFile4,real4Matrix,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal4)


!  3) array
!  ---------
   CALL defineFileName(outputFile4,'fort.4320')
   nbOfWord=-1
   CALL writeOnDisk(ioFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal4)

   CALL defineFileName(outputFile4,'fort.4330')
   nbOfWord=-1
   CALL writeOnDisk(ioFile4,real4Array,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal4)


!  Test for real(kind=8)
!  ---------------------
   CALL createFile(outputFile8,'fort.8120',formType=GHER_FORMATTED)
   CALL createFile(ioFile8,'temp.real8',formType=GHER_UNFORMATTED)

!  1) vector
!  ---------
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile8,real8Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile8,ptrVectorReal8,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile8,ptrVectorReal8,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal8)

   CALL defineFileName(outputFile8,'fort.8130')
   nbOfWord=iminusone*nbOfDataDegenerated
   CALL writeOnDisk(ioFile8,real8Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile8,ptrVectorReal8,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile8,ptrVectorReal8,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal8)

!  2) matrix
!  ---------

   CALL defineFileName(outputFile8,'fort.8220')
   nbOfWord=-1
   CALL writeOnDisk(ioFile8,real8Matrix,exclusionValue,nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile8,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile8,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal8)

   CALL defineFileName(outputFile8,'fort.8230')
   nbOfWord=iminusone
   CALL writeOnDisk(ioFile8,real8Matrix,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile8,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile8,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal8)


!  3) array
!  ---------
   CALL defineFileName(outputFile8,'fort.8320')
   nbOfWord=-1
   CALL writeOnDisk(ioFile8,real8Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile8,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile8,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal8)

   CALL defineFileName(outputFile8,'fort.8330')
   nbOfWord=-1
   CALL writeOnDisk(ioFile8,real8Array,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile8,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile8,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal8)


!  Test for real(kind=8) and real(kind=4) crossed
!  ----------------------------------------------

   CALL defineFileName(outputFile8,'fort.4329')
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile4,ptrVectorReal8,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile8,ptrVectorReal8,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal8)

   CALL defineFileName(outputFile4,'fort.4430')
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile8,real8Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile8,ptrVectorReal4,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile4,ptrVectorReal4,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal4)

   CALL defineFileName(outputFile8,'fort.8431')
   CALL writeOnDisk(ioFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ)
   CALL readFromDisk(ioFile4,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile8,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal8)

   CALL defineFileName(outputFile4,'fort.4431')
   CALL writeOnDisk(ioFile4,real8Matrix,exclusionValue,nbOfDataI,nbOfDataJ)
   CALL readFromDisk(ioFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile4,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal4)

   CALL defineFileName(outputFile8,'fort.8432')
   CALL writeOnDisk(ioFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
   CALL readFromDisk(ioFile4,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile8,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal8)

   CALL defineFileName(outputFile4,'fort.4432')
   CALL writeOnDisk(ioFile4,real8Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
   CALL readFromDisk(ioFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile4,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal4)

   CALL defineFileName(outputFile4,'fort.4433')
   nbOfWord=iminusone*nbOfDataDegenerated
   CALL writeOnDisk(ioFile4,real4Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile4,ptrVectorReal8,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile4,ptrVectorReal8,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal8)

   CALL defineFileName(outputFile8,'fort.8433')
   nbOfWord=iminusone*nbOfDataDegenerated
   CALL writeOnDisk(ioFile8,real8Vector,exclusionValue,iminusone*nbOfDataDegenerated,nbOfWord)
   CALL readFromDisk(ioFile8,ptrVectorReal4,readExclusionValue,readDimI)
   CALL writeOnDisk(outputFile8,ptrVectorReal4,readExclusionValue,readDimI)
   DEALLOCATE(ptrVectorReal4)

   CALL defineFileName(outputFile4,'fort.4434')
   nbOfWord=iminusone
   CALL writeOnDisk(ioFile4,real4Matrix,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile4,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile4,ptrMatrixReal8,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal8)

   CALL defineFileName(outputFile8,'fort.8434')
   nbOfWord=iminusone
   CALL writeOnDisk(ioFile8,real8Matrix,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfWord)
   CALL readFromDisk(ioFile8,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   CALL writeOnDisk(outputFile8,ptrMatrixReal4,readExclusionValue,readDimI,readDimJ)
   DEALLOCATE(ptrMatrixReal4)

   CALL defineFileName(outputFile4,'fort.4435')
   nbOfWord=-1
   CALL writeOnDisk(ioFile4,real4Array,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile4,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile4,ptrArrayReal8,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal8)

   CALL defineFileName(outputFile8,'fort.8435')
   nbOfWord=-1
   CALL writeOnDisk(ioFile8,real8Array,exclusionValue,iminusone*nbOfDataI,nbOfDataJ,nbOfDataK,nbOfWord)
   CALL readFromDisk(ioFile8,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   CALL writeOnDisk(outputFile8,ptrArrayReal4,readExclusionValue,readDimI,readDimJ,readDimK)
   DEALLOCATE(ptrArrayReal4)

! Test compatibility with old version
! -----------------------------------
!   OPEN(unit=15,file='fort.old.412',form='unformatted')
!   CALL uwritc(15,real8Vector,real4Vector,exclusionValue,4,4,1,1,nbOfWord)
!   CLOSE(15)
!   OPEN(unit=15,file='fort.old.812',form='unformatted')
!   CALL uwritc(15,real8Vector,real4Vector,exclusionValue,4,4,1,1,nbOfWord)
!   CLOSE(15)

!  1) vector
!  ---------

   CALL defineFileName(outputFile4,'fort.old.41')
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile4,real4Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   OPEN(unit=15,file='temp.real4',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile4,vector4,readExclusionValue,readDimI)

   CALL defineFileName(outputFile8,'fort.old.81')
   nbOfWord=nbOfDataDegenerated
   CALL writeOnDisk(ioFile8,real8Vector,exclusionValue,nbOfDataDegenerated,nbOfWord)
   OPEN(unit=15,file='temp.real8',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile8,vector8,readExclusionValue,readDimI)


!  2) matrix
!  ---------

   CALL defineFileName(outputFile4,'fort.old.42')
   CALL writeOnDisk(ioFile4,real4Matrix,exclusionValue,nbOfDataI,nbOfDataJ)
   OPEN(unit=15,file='temp.real4',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile4,vector4,readExclusionValue,readDimI*readDimJ)

   CALL defineFileName(outputFile8,'fort.old.82')
   CALL writeOnDisk(ioFile8,real8Matrix,exclusionValue,nbOfDataI,nbOfDataJ)
   OPEN(unit=15,file='temp.real8',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile8,vector8,readExclusionValue,readDimI*readDimJ)

!  3) array
!  ---------

   CALL defineFileName(outputFile4,'fort.old.43')
   CALL writeOnDisk(ioFile4,real4Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
   OPEN(unit=15,file='temp.real4',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile4,vector4,readExclusionValue,readDimI*readDimJ*readDimK)

   CALL defineFileName(outputFile8,'fort.old.83')
   CALL writeOnDisk(ioFile8,real8Array,exclusionValue,nbOfDataI,nbOfDataJ,nbOfDataK)
   OPEN(unit=15,file='temp.real8',form='unformatted')
   CALL ureadc(15,vector8,vector4,readExclusionValue,iprecision,readDimI,readDimJ,readDimK,nbOfWord)
   CLOSE(15)
   CALL writeOnDisk(outputFile8,vector8,readExclusionValue,readDimI*readDimJ*readDimK)

!  Always finalise the DIVA context
!  ================================
   CALL finaliseDIVAContext()

END PROGRAM testUReadc

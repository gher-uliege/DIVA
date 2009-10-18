PROGRAM griddef

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
   INTEGERType :: nbOfDataX, nbOfDataY
   REALType    :: originX, originY, deltaX, deltaY, exclusionValue

   TYPE(file) :: outputFile, inputFile

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

!    Main procedure
!    --------------
!       Opening file to read and reading data
!       -------------------------------------
   CALL createFile(inputFile,'fort.13',getLogicalUnit())
   CALL openFile(inputFile)
   inputFileUnit = getFileUnit(inputFile)
   
   CALL readData(inputFileUnit, nbOfDataX, nbOfDataY, originX, originY, deltaX, deltaY, exclusionValue)
   CALL closeFile(inputFile)

!       Opening file to write and writing data
!       --------------------------------------
   CALL createFile(outputFile,'fort.13',getLogicalUnit())
   CALL openFile(outputFile)
   outputFileUnit = getFileUnit(outputFile)

   CALL writeData(outputFileUnit, nbOfDataX, nbOfDataY, originX, originY, deltaX, deltaY, exclusionValue)
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

! Procedure 1 : read data
! -----------------------
 SUBROUTINE readData(inputFileUnit, nbOfDataX, nbOfDataY, originX, originY, deltaX, deltaY, exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: inputFileUnit
      INTEGERType, INTENT(OUT) :: nbOfDataX, nbOfDataY
      REALType, INTENT(OUT)    :: originX, originY, deltaX, deltaY, exclusionValue

!     Body
!     - - -
      READ(inputFileUnit,*) originX
      READ(inputFileUnit,*) originY
      READ(inputFileUnit,*) deltaX
      READ(inputFileUnit,*) deltaY
      READ(inputFileUnit,*) nbOfDataX
      READ(inputFileUnit,*) nbOfDataY
      READ(inputFileUnit,*) exclusionValue

 END SUBROUTINE

! Procedure 2 : write data
! ------------------------
 SUBROUTINE writeData(outputFileUnit, nbOfDataX, nbOfDataY, originX, originY, deltaX, deltaY, exclusionValue)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: outputFileUnit
      INTEGERType, INTENT(IN) :: nbOfDataX, nbOfDataY
      REALType, INTENT(IN)    :: originX, originY, deltaX, deltaY, exclusionValue

!     Body
!     - - -
      WRITE(outputFileUnit,*) originX - deltaX
      WRITE(outputFileUnit,*) originY - deltaY
      WRITE(outputFileUnit,*) deltaX
      WRITE(outputFileUnit,*) deltaY
      WRITE(outputFileUnit,*) nbOfDataX
      WRITE(outputFileUnit,*) nbOfDataY
      WRITE(outputFileUnit,*) exclusionValue

 END SUBROUTINE

END PROGRAM  griddef

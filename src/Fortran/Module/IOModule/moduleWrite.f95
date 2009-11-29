MODULE moduleWrite

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module specifications               ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================

! Include file
! ============
   USE moduleFileDefinition
   USE moduleFileFormatType, ONLY : getFileFormatType
   USE moduleIOBase, ONLY : setFile

#ifdef _REAL_
   USE moduleWriteGHER, ONLY : writeVectorGHER => writeVector, &
                               writeMatrixGHER => writeMatrix, &
                               writeArrayGHER => writeArray
#endif

   USE moduleWriteTHK, ONLY : writeVectorTHK => writeVector,&
                              writeMatrixTHK => writeMatrix, &
                              writeArrayTHK => writeArray

   INCLUDE 'constantParameter.h'

! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: writeVector, writeMatrix, writeArray

! ============================================================
! ============================================================
! ============================================================
! ===                                                      ===
! ===                                                      ===
! ===                  Module procedures                   ===
! ===                                                      ===
! ===                                                      ===
! ============================================================
! ============================================================
! ============================================================
 CONTAINS

! =============================================================
! ===            Internal procedure ("PUBLIC")  : Others    ===
! =============================================================

! Procedure 1 : generic write for vector
! --------------------------------------
  SUBROUTINE writeVector(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX,&
                                exclusionValue,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:), INTENT(IN) :: entries
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      LOGICAL, INTENT(IN) :: fileOpened
      INTEGER :: formatOfTheFile, ichoice
!     Body
!     - - -
      formatOfTheFile = getFileFormatType(fileToWrite%formatFile)

      SELECT CASE (formatOfTheFile)
         CASE (ione) ! GHER_FORMATTED
               ichoice = ione
         CASE (itwo) ! GHER_UNFORMATTED
               ichoice = ione
         CASE (ithree) ! THK_FORMATTED
               ichoice = itwo
         CASE (ifour) ! THK_UNFORMATTED
               ichoice = itwo
         CASE DEFAULT
               ichoice = itwo
      END SELECT

      SELECT CASE (ichoice)
         CASE (ione) ! GHER
#ifdef _REAL_
               CALL writeVectorGHER(fileToWrite,entries,exclusionValue,nbOfDataX)
#else
               PRINT*,'No writing procedure for integer with GHER format'
#endif

         CASE (itwo) ! THK
               CALL writeVectorTHK(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX,fileOpened)
      END SELECT

  END SUBROUTINE

! Procedure 2 : generic write for matrix
! --------------------------------------
  SUBROUTINE writeMatrix(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX,&
                                                    increaseSizeY,nbOfDataY,firstIndexY,lastIndexY,&
                                                    exclusionValue,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:,:), INTENT(IN) :: entries
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      INTEGER, INTENT(IN) :: increaseSizeY,nbOfDataY,firstIndexY,lastIndexY
      LOGICAL, INTENT(IN) :: fileOpened
      INTEGER :: formatOfTheFile, ichoice
!     Body
!     - - -
      formatOfTheFile = getFileFormatType(fileToWrite%formatFile)

      SELECT CASE (formatOfTheFile)
         CASE (ione) ! GHER_FORMATTED
               ichoice = ione
         CASE (itwo) ! GHER_UNFORMATTED
               ichoice = ione
         CASE (ithree) ! THK_FORMATTED
               ichoice = itwo
         CASE (ifour) ! THK_UNFORMATTED
               ichoice = itwo
         CASE DEFAULT
               ichoice = itwo
      END SELECT

      SELECT CASE (ichoice)
         CASE (ione) ! GHER
#ifdef _REAL_
               CALL writeMatrixGHER(fileToWrite,entries,exclusionValue,nbOfDataX,nbOfDataY)
#else
               PRINT*,'No writing procedure for integer with GHER format'
#endif

         CASE (itwo) ! THK
               CALL writeMatrixTHK(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX, &
                                                       increaseSizeY,nbOfDataY,firstIndexY,lastIndexY,fileOpened)
      END SELECT

  END SUBROUTINE

! Procedure 3 : generic write for array
! --------------------------------------
  SUBROUTINE writeArray(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX,&
                                                   increaseSizeY,nbOfDataY,firstIndexY,lastIndexY,&
                                                   increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ,&
                                                   exclusionValue,fileOpened)

!     Declaration
!     - - - - - -
      TYPE(file) :: fileToWrite
      REAL(KIND=4), INTENT(IN) :: exclusionValue
      VARType, DIMENSION(:,:,:), INTENT(IN) :: entries
      INTEGER, INTENT(IN) :: increaseSizeX,nbOfDataX,firstIndexX,lastIndexX
      INTEGER, INTENT(IN) :: increaseSizeY,nbOfDataY,firstIndexY,lastIndexY
      INTEGER, INTENT(IN) :: increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ
      LOGICAL, INTENT(IN) :: fileOpened
      INTEGER :: formatOfTheFile, ichoice
!     Body
!     - - -
      formatOfTheFile = getFileFormatType(fileToWrite%formatFile)

      SELECT CASE (formatOfTheFile)
         CASE (ione) ! GHER_FORMATTED
               ichoice = ione
         CASE (itwo) ! GHER_UNFORMATTED
               ichoice = ione
         CASE (ithree) ! THK_FORMATTED
               ichoice = itwo
         CASE (ifour) ! THK_UNFORMATTED
               ichoice = itwo
         CASE DEFAULT
               ichoice = itwo
      END SELECT

      SELECT CASE (ichoice)
         CASE (ione) ! GHER
#ifdef _REAL_
               CALL writeArrayGHER(fileToWrite,entries,exclusionValue,nbOfDataX,nbOfDataY,nbOfDataZ)
#else
               PRINT*,'No writing procedure for integer with GHER format'
#endif

         CASE (itwo) ! THK
               CALL writeArrayTHK(fileToWrite,entries,increaseSizeX,nbOfDataX,firstIndexX,lastIndexX, &
                                                       increaseSizeY,nbOfDataY,firstIndexY,lastIndexY, &
                                                       increaseSizeZ,nbOfDataZ,firstIndexZ,lastIndexZ,fileOpened)
      END SELECT

  END SUBROUTINE

END MODULE moduleWrite

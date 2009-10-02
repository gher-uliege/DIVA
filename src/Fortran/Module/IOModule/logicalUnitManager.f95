MODULE logicalUnitManager
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
   include 'logicalUnit.h'
   include 'logicalParameter.h'
   
! Declaration
! ===========
   INTEGER, PRIVATE, PARAMETER :: iDefaultStart = 100
   INTEGER, PRIVATE, PARAMETER :: iDefaultEnd = 900
   INTEGER, PRIVATE :: numberOfData
   LOGICAL, PRIVATE :: isInitialise = false

   TYPE (logicalUnit), PRIVATE, POINTER :: workingLogicalUnit => NULL()
   TYPE (logicalUnit), PRIVATE, DIMENSION(:), ALLOCATABLE :: dataBase

! Procedures status
! =================
   PUBLIC :: initialiseDefault, initialise, destructor, getLogicalUnit
   PRIVATE :: setNumberOfData, createDataBase, initialiseDataBase, searchFreeLogicalUnit, setWorkingLogicalUnit

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
! ============================================================
! ===            External procedure ("PUBLIC")             ===
! ============================================================
! Procedure 1 : initialisation of the database
! --------------------------------------------
   SUBROUTINE initialise(iStart,iEnd)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iStart, iEnd

!     Body
!     - - -
      CALL setNumberOfData(iEnd-iStart+1)
      CALL createDataBase()
      CALL initialiseDataBase(iStart,iEnd)

      isInitialise = true ;
      
   END SUBROUTINE

! Procedure 2 : destructor of the database
! ----------------------------------------
   SUBROUTINE destructor()

!     Body
!     - - -
      IF ( isInitialise ) THEN
          DEALLOCATE(dataBase)
      END IF
      isInitialise = false
      
   END SUBROUTINE
   
! Procedure 3 : allocation of a logical unit on request
! -----------------------------------------------------
   FUNCTION getLogicalUnit() RESULT(number)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), POINTER :: number

!     Body
!     - - -
      IF ( .NOT.(isInitialise) ) THEN
          CALL initialiseDefault()
      END IF
      CALL searchFreeLogicalUnit(number)

   END FUNCTION
   
! Procedure 4 : initialisation of the database
! --------------------------------------------
   SUBROUTINE initialiseDefault()

!     Body
!     - - -
      CALL initialise(iDefaultStart,iDefaultEnd)

   END SUBROUTINE

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================
! Procedure 1 : defining the number of Data
! -----------------------------------------
   SUBROUTINE setNumberOfData(num)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: num

!     Body
!     - - -
      numberOfData = num ;

   END SUBROUTINE
    
! Procedure 2 : creating the database vector
! ------------------------------------------
   SUBROUTINE createDataBase()

!     Body
!     - - -
      SELECT CASE (allocated(dataBase))
      CASE (true)
         CALL destructor()
      CASE DEFAULT
      END SELECT
      
      ALLOCATE(dataBase(numberOfData))
      
   END SUBROUTINE
   
! Procedure 3 : initialisation of the database
! --------------------------------------------
   SUBROUTINE initialiseDataBase(iStart,iEnd)
   
!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: iStart, iEnd
      INTEGER :: i1

!     Body
!     - - -
      DO i1 = 1, numberOfData
         CALL setWorkingLogicalUnit(dataBase(i1))
         workingLogicalUnit%unit = iStart + i1 - 1
         workingLogicalUnit%isUsed = false
      END DO

   END SUBROUTINE
   
! Procedure 4 : Searching for free logical unit
! ---------------------------------------------
   SUBROUTINE searchFreeLogicalUnit(number)

!     Declaration
!     - - - - - -
      INTEGER :: i1
      TYPE (logicalUnit), INTENT(OUT), POINTER :: number

!     Body
!     - - -
      DO i1 = 1, numberOfData
         CALL setWorkingLogicalUnit(dataBase(i1))
         IF ( .NOT.(workingLogicalUnit%isUsed) ) EXIT
      END DO

      workingLogicalUnit%isUsed = true
      number => workingLogicalUnit

   END SUBROUTINE
   
! Procedure 5 : setting pointer to logicalUnit
! --------------------------------------------
   SUBROUTINE setWorkingLogicalUnit(baseLogicalUnit)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), INTENT(IN), TARGET :: baseLogicalUnit

!     Body
!     - - -
      workingLogicalUnit => baseLogicalUnit

   END SUBROUTINE
   
END MODULE logicalUnitManager

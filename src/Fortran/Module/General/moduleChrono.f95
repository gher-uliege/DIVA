MODULE moduleChrono
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
#ifndef _MODULE_CHRONO_
#define _MODULE_CHRONO_
   include 'chrono.h'
   include 'ioParameter.h'
   include 'logicalParameter.h'
#endif

! Declaration
! ===========
   Type(Chronometer), PRIVATE :: internalChronometer
   Type(Chronometer), PRIVATE, POINTER :: workingChronometer => NULL()

! Procedures status
! =================
   PUBLIC :: startChrono, finishChrono, printInformationChrono , initialise
   PRIVATE :: startingChronometer, finishingChronometer, printInformationChronometer, setWorkingChronometer

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

! Procedure 1 : initialise
! -------------------------
   SUBROUTINE initialise()

!     Body
!     - - -
    internalChronometer=Chronometer((/0,0,0,0,0,0,0,0/),(/0,0,0,0,0,0,0,0/),true)

   END SUBROUTINE
   
! Procedure 2 : starting the Chronometer
! --------------------------------------
   SUBROUTINE startChrono(chrono)

!     Declaration
!     - - - - - -
      Type(Chronometer), OPTIONAL, INTENT(IN) :: chrono

!     Body
!     - - -
      IF ( PRESENT(chrono) ) THEN
         CALL setWorkingChronometer(chrono)
      ELSE
         CALL setWorkingChronometer(internalChronometer)
      END IF

      CALL startingChronometer()

   END SUBROUTINE

! Procedure 3 : finalisation of the context DIVA
! ----------------------------------------------
   SUBROUTINE finishChrono(chrono)

!     Declaration
!     - - - - - -
      Type(Chronometer), OPTIONAL, INTENT(IN) :: chrono

!     Body
!     - - -
      IF ( PRESENT(chrono) ) THEN
         CALL setWorkingChronometer(chrono)
      ELSE
         CALL setWorkingChronometer(internalChronometer)
      END IF

      CALL finishingChronometer()

   END SUBROUTINE

! Procedure 4 : printing ellapsed time between starting and finish point
! ----------------------------------------------------------------------
   SUBROUTINE printInformationChrono(chrono)

!     Declaration
!     - - - - - -
      Type(Chronometer), OPTIONAL, INTENT(IN) :: chrono

!     Body
!     - - -
      IF ( PRESENT(chrono) ) THEN
         CALL setWorkingChronometer(chrono)
      ELSE
         CALL setWorkingChronometer(internalChronometer)
      END IF

      CALL printInformationChronometer()

   END SUBROUTINE

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================

! Procedure 1 : starting the Chronometer
! --------------------------------------
   SUBROUTINE startingChronometer()

!     Body
!     - - -
      CALL date_and_time(values=workingChronometer%timeDataInit)
      workingChronometer%isStarted = true

   END SUBROUTINE

! Procedure 2 : finishing the Chronometer
! --------------------------------------
   SUBROUTINE finishingChronometer()

!     Body
!     - - -
      CALL date_and_time(values=workingChronometer%timeDataEnd)
      workingChronometer%isStarted = false

   END SUBROUTINE

! Procedure 3 : printing ellapsed time between starting and finish point
! ----------------------------------------------------------------------
   SUBROUTINE printInformationChronometer()

!     Declaration
!     - - - - - -
      REAL*8 :: startingPoint, finishPoint

!     Body
!     - - -
      startingPoint = workingChronometer%timeDataInit(5) * 3600.D+0 +workingChronometer%timeDataInit(6) * 60.D+0 &
                    + workingChronometer%timeDataInit(7) + 1.D-3 * workingChronometer%timeDataInit(8)
      finishPoint = workingChronometer%timeDataEnd(5) * 3600.D+0 +workingChronometer%timeDataEnd(6) * 60.D+0 &
                    + workingChronometer%timeDataEnd(7) + 1.D-3 * workingChronometer%timeDataEnd(8)
      WRITE(stdOutput,*) 'Ellapsed time : ', finishPoint - startingPoint , ' seconds'

   END SUBROUTINE

! Procedure 4 : setting working chronometer
! -----------------------------------------
   SUBROUTINE setWorkingChronometer(chrono)

!     Declaration
!     - - - - - -
      TYPE(Chronometer), INTENT(IN), TARGET :: chrono

!     Body
!     - - -
      workingChronometer => chrono

   END SUBROUTINE

END MODULE moduleChrono

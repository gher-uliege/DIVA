MODULE moduleFileFormatType
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

! Declaration
! ===========
#ifndef _MODULE_FILE_FORMAT_TYPE_
#define _MODULE_FILE_FORMAT_TYPE_
   INCLUDE 'fileFormatType.h'
#endif

   TYPE(fileFormatType), PUBLIC, PARAMETER :: GHER_FORMATTED = fileFormatType(1,.TRUE.)
   TYPE(fileFormatType), PUBLIC, PARAMETER :: GHER_UNFORMATTED = fileFormatType(2,.FALSE.)
   TYPE(fileFormatType), PUBLIC, PARAMETER :: GHER = fileFormatType(2,.FALSE.)

! Procedures status
! =================
   PUBLIC :: getFileFormatType, getFileForm
   
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

! ===========================================================
! ===            Internal procedure ("PUBLIC")            ===
! ===========================================================

! Procedure 1 : get the file format type
! --------------------------------------
  FUNCTION getFileFormatType(formatSelection) RESULT(ivalue)

!     Declaration
!     - - - - - -
      INTEGER :: ivalue
      TYPE(fileFormatType), INTENT(IN) :: formatSelection

!     Body
!     - - -
      ivalue = formatSelection%fileFormatTypeValue

  END FUNCTION

! Procedure 2 : get the form of the file (formatted or unformatted)
! --------------------------------------
  FUNCTION getFileForm(formatSelection) RESULT(ivalue)

!     Declaration
!     - - - - - -
      LOGICAL :: ivalue
      TYPE(fileFormatType), INTENT(IN) :: formatSelection

!     Body
!     - - -
      ivalue = formatSelection%formType

  END FUNCTION

END MODULE moduleFileFormatType




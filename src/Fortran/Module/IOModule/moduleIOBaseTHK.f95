MODULE moduleIOBaseTHK

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
  USE moduleIOBase, ONLY : getFileFormType

  INCLUDE 'logicalParameter.h'
  INCLUDE 'constantParameter.h'
  INCLUDE 'ioParameter.h'


! Procedures status
! =================

!  General part
!  ------------
   PUBLIC :: writeDimension, readDimension, writeType, readType, writePrecision, readPrecision
   PRIVATE :: writeOneData, readOneData


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
! ===            Internal procedure ("PUBLIC")  : Others  ===
! ===========================================================

! Procedure 1 : write dimension
! -----------------------------
  FUNCTION writeDimension(unitToWrite,increaseSize,nbOfData,firstIndex,lastIndex) RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToWrite,increaseSize,nbOfData,firstIndex,lastIndex
      INTEGER :: icheckError
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           WRITE(unitToWrite,*,ERR=99) increaseSize,nbOfData,firstIndex,lastIndex
        CASE (false)
           WRITE(unitToWrite,ERR=99) increaseSize,nbOfData,firstIndex,lastIndex
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 2 : read dimension
! ----------------------------
  SUBROUTINE readDimension(unitToRead,increaseSize,nbOfData,firstIndex,lastIndex,icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToRead
      INTEGER, INTENT(OUT) :: icheckError,increaseSize,nbOfData,firstIndex,lastIndex
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           READ(unitToRead,*,ERR=99,END=99) increaseSize,nbOfData,firstIndex,lastIndex
        CASE (false)
           READ(unitToRead,ERR=99,END=99) increaseSize,nbOfData,firstIndex,lastIndex
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! Procedure 3 : write one data
! ----------------------------
  FUNCTION writeOneData(unitToWrite,data) RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToWrite,data
      INTEGER :: icheckError
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           WRITE(unitToWrite,*,ERR=99) data
        CASE (false)
           WRITE(unitToWrite,ERR=99) data
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END FUNCTION

! Procedure 4 : read one data
! ---------------------------
  SUBROUTINE readOneData(unitToRead,data,icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToRead
      INTEGER, INTENT(OUT) :: icheckError,data
      LOGICAL :: formType

!     Body
!     - - -
      icheckError = izero
      formType = getFileFormType()

      SELECT CASE (formType)
        CASE (true)
           READ(unitToRead,*,ERR=99,END=99) data
        CASE (false)
           READ(unitToRead,ERR=99,END=99) data
      END SELECT

      RETURN

99    CONTINUE
      icheckError = ione

   END SUBROUTINE

! Procedure 5 : write type
! ------------------------
  FUNCTION writeType(unitToWrite,type) RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToWrite,type
      INTEGER :: icheckError

!     Body
!     - - -
      icheckError = writeOneData(unitToWrite,type)

   END FUNCTION

! Procedure 6 : read type
! -----------------------
  SUBROUTINE readType(unitToRead,type,icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToRead
      INTEGER, INTENT(OUT) :: icheckError,type

!     Body
!     - - -
      CALL readOneData(unitToRead,type,icheckError)

   END SUBROUTINE

! Procedure 7 : write precision
! -----------------------------
  FUNCTION writePrecision(unitToWrite,prec) RESULT (icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToWrite,prec
      INTEGER :: icheckError

!     Body
!     - - -
      icheckError = writeOneData(unitToWrite,prec)

   END FUNCTION

! Procedure 8 : read precision
! ----------------------------
  SUBROUTINE readPrecision(unitToRead,prec,icheckError)

!     Declaration
!     - - - - - -
      INTEGER, INTENT(IN) :: unitToRead
      INTEGER, INTENT(OUT) :: icheckError,prec

!     Body
!     - - -
      CALL readOneData(unitToRead,prec,icheckError)

   END SUBROUTINE


END MODULE moduleIOBaseTHK

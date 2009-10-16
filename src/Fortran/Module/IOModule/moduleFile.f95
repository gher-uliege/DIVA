MODULE moduleFile
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
   USE logicalUnitManager, getLogicalUnitFromUnitManager => getLogicalUnit
   include 'fileType.h'

! Declaration
! ===========
   TYPE (file), PRIVATE, POINTER :: workingFile => NULL()

! Procedures status
! =================
   PUBLIC :: initialiseFile, defineFile, defineFileName, defineLogicalUnit, printInformation, openFile, closeFile, isFileOpened, &
             createFile, getFileUnit, defineFileFormat, Formatted, Unformatted, getFileFormat
   PRIVATE :: setWorkingFile, internalInitialise, internalDefineFileName, internalDefineLogicalUnit, internalPrintInformation, &
              setIsLinked, isFileLinked, getLogicalUnit, getFileName, setIsOpened, internalIsFileOpened, nullify, &
              internalDefineFormType

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

! Procedure 1 : initialisation of the file (to be called for each 'file' created)
! ----------------------------------------
   SUBROUTINE initialiseFile(targetFile)
   
!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalInitialise()
      
!     Nullify pointer
!     - - - - - - - -
      CALL nullify()
      
   END SUBROUTINE

! Procedure 2 : defining file name
! --------------------------------
   SUBROUTINE defineFile(targetFile,name,unit,formType)

!     Declaration
!     - - - - - -
      CHARACTER(*), OPTIONAL, INTENT(IN) :: name
      TYPE(logicalUnit), OPTIONAL, POINTER, INTENT(IN)     :: unit
      LOGICAL, OPTIONAL, INTENT(IN) :: formType

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      IF ( PRESENT(name) ) THEN
         CALL internalDefineFileName(name)
      END IF
      IF ( PRESENT(unit) ) THEN
         CALL internalDefineLogicalUnit(unit)
      END IF
      IF ( PRESENT(formType) ) THEN
         CALL internalDefineFormType(formType)
      ENDIF

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE


! Procedure 3 : defining file name
! --------------------------------
   SUBROUTINE defineFileName(targetFile,name)
   
!     Declaration
!     - - - - - -
      CHARACTER(*), INTENT(IN) :: name

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalDefineFileName(name)
      
!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 4 : link a logical unit to the file
! ---------------------------------------------
   SUBROUTINE defineLogicalUnit(targetFile,unit)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), OPTIONAL, POINTER, INTENT(IN)     :: unit

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalDefineLogicalUnit(unit)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 5 : print information on the file
! -------------------------------------------
   SUBROUTINE printInformation(targetFile)

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalPrintInformation()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 6 : open a logical unit
! ---------------------------------
   SUBROUTINE openFile(targetFile,checkError)

!     Declaration
!     - - - - - -
      INTEGER :: checkValue
      LOGICAL, OPTIONAL, INTENT(OUT) :: checkError

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(IN) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      SELECT CASE (isFileLinked())
      CASE (true)
            IF ( workingFile%formatFile ) THEN
                 OPEN(unit=getLogicalUnit(),file=getFileName(),iostat=checkValue,form='formatted')
            ELSE
                 OPEN(unit=getLogicalUnit(),file=getFileName(),iostat=checkValue,form='unformatted')
            END IF

            IF ( PRESENT(checkError) ) THEN
               IF ( checkValue == 0 ) THEN
                   checkError = true
                   CALL setIsOpened(true)
               ELSE
                   checkError = false
                   CALL setIsOpened(false)
               END IF
            END IF
            
      CASE (false)
          checkError = false
          CALL setIsOpened(false)
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 7 : close a logical unit
! ----------------------------------
   SUBROUTINE closeFile(targetFile,checkError)

!     Declaration
!     - - - - - -
      INTEGER :: checkValue
      LOGICAL, OPTIONAL, INTENT(OUT) :: checkError

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(IN) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      SELECT CASE (isFileLinked())
      CASE (true)
            CLOSE(unit=getLogicalUnit(),iostat=checkValue)

            IF ( PRESENT(checkError) ) THEN
               IF ( checkValue == 0 ) THEN
                   checkError = true
                   CALL setIsOpened(false)
               ELSE
                   checkError = false
               END IF
            END IF

      CASE (false)
          checkError = false
      END SELECT

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE
   
! Procedure 8 : information of the "opening" status of the file
! -------------------------------------------------------------
   FUNCTION isFileOpened(targetFile) RESULT(check)

!     Declaration
!     - - - - - -
      LOGICAL :: check

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      check = internalIsFileOpened()

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END FUNCTION

! Procedure 9 : creating new file
! -------------------------------
   SUBROUTINE createFile(targetFile,name,unit,formType)

!     Declaration
!     - - - - - -
      CHARACTER(*), OPTIONAL, INTENT(IN) :: name
      TYPE(logicalUnit), OPTIONAL, POINTER, INTENT(IN)     :: unit
      TYPE(file), INTENT(INOUT) :: targetFile
      LOGICAL, OPTIONAL, INTENT(IN) :: formType

!     Body
!     - - -
      CALL initialiseFile(targetFile)
      CALL defineFile(targetFile,name,unit,formType)

   END SUBROUTINE

! Procedure 10 : get the logical unit number of the file
! ------------------------------------------------------
   FUNCTION getFileUnit(targetFile) RESULT(unit1)

!     Declaration
!     - - - - - -
      INTEGER :: unit1

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(IN) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      unit1 = getLogicalUnit()

   END FUNCTION

! Procedure 11 : defining file format
! -----------------------------------
   SUBROUTINE defineFileFormat(targetFile,fileFormat)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: fileFormat

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalDefineFormType(fileFormat)

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 12 : set formatted format
! ----------------------------------
  FUNCTION Formatted() RESULT(choice)

!     Declaration
!     - - - - - -
      LOGICAL :: choice

!     Body
!     - - -
      choice = true

  END FUNCTION

! Procedure 13 : set unformatted format
! ----------------------------------
  FUNCTION Unformatted() RESULT(choice)

!     Declaration
!     - - - - - -
      LOGICAL :: choice

!     Body
!     - - -
      choice = false

  END FUNCTION

! Procedure 14 : obtain the output format
! ----------------------------------------
  FUNCTION getFileFormat(targetFile) RESULT(choice)

!     Declaration
!     - - - - - -
      LOGICAL :: choice

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(IN) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      choice = workingFile%formatFile

  END FUNCTION

! ============================================================
! ===            Internal procedure ("PRIVATE")            ===
! ============================================================

! Procedure 1 : setting pointer to file
! -------------------------------------
   SUBROUTINE setWorkingFile(targetFile)
   
!     Declaration
!     - - - - - -
      TYPE(file), INTENT(IN), TARGET :: targetFile

!     Body
!     - - -
      workingFile => targetFile
      
   END SUBROUTINE


! Procedure 2 : file initialisation
! ---------------------------------
   SUBROUTINE internalInitialise()

!     Body
!     - - -
      CALL internalDefineLogicalUnit()
      CALL internalDefineFileName(' ')
      CALL internalDefineFormType(true)
      CALL setIsLinked(false)
      CALL setIsOpened(false)

   END SUBROUTINE

! Procedure 3 : file's name definition
! ----------------------------------
   SUBROUTINE internalDefineFileName(name)

!     Declaration
!     - - - - - -
      CHARACTER(*), INTENT(IN) :: name

!     Body
!     - - -
      workingFile%fileName = name

   END SUBROUTINE

! Procedure 4 : file's logical unit definition
! --------------------------------------------
   SUBROUTINE internalDefineLogicalUnit(unit)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), OPTIONAL, POINTER, INTENT(IN)     :: unit

!     Body
!     - - -
      IF ( PRESENT(unit) ) THEN
           workingFile%logicalUnit => unit
      ELSE
           workingFile%logicalUnit => NULL()
      END IF
      
      CALL setIsLinked(true)

   END SUBROUTINE

! Procedure 5 : define linked status of the file to logical unit
! --------------------------------------------------------------
   SUBROUTINE setIsLinked(booleanValue)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: booleanValue
      
!     Body
!     - - -
      workingFile%isLinked = booleanValue
      
   END SUBROUTINE
   
! Procedure 6 : print information on the file
! -------------------------------------------
   SUBROUTINE internalPrintInformation()

!     Body
!     - - -
      PRINT*,'Name of the file : ', workingFile%fileName
      SELECT CASE (isFileLinked())
      CASE (true)
         PRINT*,'   The file is linked to the logical unit : ', getLogicalUnit()
      CASE (false)
         PRINT*,'   The file is not linked to a logical unit.'
      END SELECT
      PRINT*,' '

   END SUBROUTINE

! Procedure 7 : logical test to know if the file is linked or not to a logical unit
! ---------------------------------------------------------------------------------
   FUNCTION isFileLinked() RESULT(check)

!     Declaration
!     - - - - - -
      LOGICAL :: check

!     Body
!     - - -
      check = workingFile%isLinked

   END FUNCTION

! Procedure 8 : get the logical unit number
! -----------------------------------------
   FUNCTION getLogicalUnit() RESULT(unit1)

!     Declaration
!     - - - - - -
      INTEGER :: unit1

!     Body
!     - - -
      unit1 = workingFile%logicalUnit%unit

   END FUNCTION

! Procedure 9 : get the logical unit number
! -----------------------------------------
   FUNCTION getFileName() RESULT(name)

!     Declaration
!     - - - - - -
      CHARACTER(LEN = fileNameMaxLength) :: name

!     Body
!     - - -
      name = workingFile%fileName

   END FUNCTION

! Procedure 10 : define opening status of the file
! -----------------------------------------------
   SUBROUTINE setIsOpened(booleanValue)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: booleanValue

!     Body
!     - - -
      workingFile%isOpened = booleanValue

   END SUBROUTINE

! Procedure 11 : information of the "opening" status of the file
! -------------------------------------------------------------
   FUNCTION internalIsFileOpened() RESULT(check)

!     Declaration
!     - - - - - -
      LOGICAL :: check

!     Body
!     - - -
      check = workingFile%isOpened

   END FUNCTION
   
! Procedure 12 : make the target of the pointer null
! --------------------------------------------------
   SUBROUTINE nullify()

!     Body
!     - - -
      workingFile => NULL()
      
   END SUBROUTINE
   
! Procedure 13 : define the formatted of the file (true = formatted, false = unformatted)
! ---------------------------------------------------------------------------------------
   SUBROUTINE internalDefineFormType(choice)

!     Declaration
!     - - - - - -
      LOGICAL, INTENT(IN) :: choice

!     Body
!     - - -
      workingFile%formatFile = choice

   END SUBROUTINE

   
END MODULE moduleFile

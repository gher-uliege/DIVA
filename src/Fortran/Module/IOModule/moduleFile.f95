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
   USE logicalUnitManager, ONLY : getLogicalUnitFromUnitManager => getLogicalUnit
   USE moduleFileFormatType
   USE moduleFileDefinition

   INCLUDE 'logicalParameter.h'

! Declaration
! ===========
   TYPE (file), PRIVATE, POINTER :: workingFile => NULL()

! Procedures status
! =================
   PUBLIC :: initialiseFile, defineFileName, printInformation, openFile, closeFile, isFileOpened, &
             createFile, getFileUnit, defineFileFormat, getFileFormat
   PRIVATE :: setWorkingFile, internalInitialise, internalDefineFileName, internalDefineLogicalUnit, internalPrintInformation, &
              setIsLinked, isFileLinked, getLogicalUnit, getFileName, setIsOpened, internalIsFileOpened, nullify, &
              internalDefineFormType, internalDefineFile, defineLogicalUnit

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

! Procedure 3 : link a logical unit to the file
! ---------------------------------------------
   SUBROUTINE defineLogicalUnit(targetFile,unit)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), OPTIONAL, POINTER     :: unit

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

! Procedure 4 : print information on the file
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

! Procedure 5 : open a logical unit
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
            IF ( workingFile%formatFile%formType ) THEN
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

! Procedure 6 : close a logical unit
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
   
! Procedure 7 : information of the "opening" status of the file
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

! Procedure 8 : creating new file
! -------------------------------
   SUBROUTINE createFile(targetFile,name,formType)

!     Declaration
!     - - - - - -
      CHARACTER(*), INTENT(IN) :: name
      TYPE(fileFormatType), OPTIONAL, INTENT(IN) :: formType

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(INOUT) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      CALL internalInitialise()

      IF ( PRESENT(formType) ) THEN
         CALL internalDefineFile(getLogicalUnitFromUnitManager(),name,formType)
      ELSE
         CALL internalDefineFile(getLogicalUnitFromUnitManager(),name,GHER)
      END IF

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END SUBROUTINE

! Procedure 9 : get the logical unit number of the file
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

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

   END FUNCTION

! Procedure 10 : defining file format
! -----------------------------------
   SUBROUTINE defineFileFormat(targetFile,fileFormat)

!     Declaration
!     - - - - - -
      TYPE(fileFormatType), INTENT(IN) :: fileFormat

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

! Procedure 11 : obtain the output format
! ----------------------------------------
  FUNCTION getFileFormat(targetFile) RESULT(choice)

!     Declaration
!     - - - - - -
      TYPE(fileFormatType) :: choice

!     Pointer filling procedure
!     - - - - - - - - - - - - -
      TYPE(file), INTENT(IN) :: targetFile
      CALL setWorkingFile(targetFile)

!     Body
!     - - -
      choice = workingFile%formatFile

!     Nullify pointer
!     - - - - - - - -
      CALL nullify()

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
      CALL internalDefineFormType(GHER)
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
   SUBROUTINE internalDefineLogicalUnit(logicalTarget)

!     Declaration
!     - - - - - -
      TYPE(logicalUnit), OPTIONAL, POINTER     :: logicalTarget

!     Body
!     - - -
      IF ( PRESENT(logicalTarget) ) THEN
           workingFile%logicalUnit%unit = logicalTarget%unit
           workingFile%logicalUnit%isUsed = logicalTarget%isUsed
      ELSE
           workingFile%logicalUnit%unit =  0
           workingFile%logicalUnit%isUsed = false
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
      CHARACTER(LEN=maxFileLengthName) :: name

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
      TYPE(fileFormatType), INTENT(IN) :: choice

!     Body
!     - - -
      workingFile%formatFile = choice

   END SUBROUTINE

! Procedure 14 : defining file name
! --------------------------------
   SUBROUTINE internalDefineFile(unit,name,formType)

!     Declaration
!     - - - - - -
      CHARACTER(*), INTENT(IN) :: name
      TYPE(logicalUnit), POINTER  :: unit
      TYPE(fileFormatType), INTENT(IN) :: formType

!     Body
!     - - -
      CALL internalDefineFileName(name)
      CALL internalDefineLogicalUnit(unit)
      CALL internalDefineFormType(formType)

   END SUBROUTINE
   

END MODULE moduleFile

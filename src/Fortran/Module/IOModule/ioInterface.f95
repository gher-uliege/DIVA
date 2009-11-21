MODULE ioInterface
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
   USE moduleReadReal4, ONLY : readDataReal4 => readData
   USE moduleReadReal8, ONLY : readDataReal8 => readData
   USE moduleWriteReal4, ONLY : writeDataReal4 => writeData
   USE moduleWriteReal8, ONLY : writeDataReal8 => writeData



! Interface
! =========
   INTERFACE ureadc
      MODULE PROCEDURE readDataReal4, readDataReal8
   END INTERFACE

   INTERFACE uwritc
      MODULE PROCEDURE writeDataReal4, writeDataReal8
   END INTERFACE

END MODULE ioInterface

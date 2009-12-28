MODULE moduleInsertValueMethod
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
#ifndef _MODULE_INSERT_VALUE_
#define _MODULE_INSERT_VALUE_
   INCLUDE 'insertValueMethod.h'
#endif

   TYPE(insertValueMethod), PUBLIC, PARAMETER :: INSERT_VALUE = insertValueMethod(1)
   TYPE(insertValueMethod), PUBLIC, PARAMETER :: ADD_VALUE = insertValueMethod(2)
   TYPE(insertValueMethod), PUBLIC, PARAMETER :: FAST_INSERT_VALUE = insertValueMethod(3)
   TYPE(insertValueMethod), PUBLIC, PARAMETER :: FAST_ADD_VALUE = insertValueMethod(4)
   TYPE(insertValueMethod), PUBLIC, PARAMETER :: SET_VALUE = insertValueMethod(5)

END MODULE moduleInsertValueMethod




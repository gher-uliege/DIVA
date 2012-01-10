#!C:/msys/local/bin/wish84.exe

#######################################################
# Main script for Diva user interface  
# Version 4.0 (November 2006)
# S. Walrave & M. Rixen 
# J.-M. Beckers, D. Sirjacobs, C. Troupin
#
# E-mail address :  ctroupin@ulg.ac.be
#######################################################


############################################################### 
# set the path to the diva-4.0 directory in your installation # 
###############################################################


set env(DIVA_GENERAL_DIR) c:/msys/home/Charles/diva-4.0


#######################################################

# 1. directory where the .tcl files are stored
# --------------------------------------------

set env(DIVA_SRC_DIR) $env(DIVA_GENERAL_DIR)/src/Tcl

# 2. "work" directory 
# -------------------

set env(DIVA_WORK_DIR) $env(DIVA_GENERAL_DIR)/GUIwork

# 3. directory where the fortran executables are stored
# -----------------------------------------------------

set env(DIVA_BIN_DIR) $env(DIVA_GENERAL_DIR)/bin


#######################################################
# Test environnement variable 
#######################################################

global auto_path  Dict
lappend auto_path $env(DIVA_SRC_DIR) .
puts "auto_path $auto_path"

source $env(DIVA_SRC_DIR)/varlist.tcl
source $env(DIVA_SRC_DIR)/dictionary.tcl
source $env(DIVA_SRC_DIR)/tablist.tcl

#######################################################
# Define X Resource File and default bindings
#######################################################

option readfile $env(DIVA_SRC_DIR)/TkDefaults
Bindings

set active_foreground [option get . activeForeground ActiveForeground]
set disable_foreground [option get . disableForeground DisableForeground]

#######################################################
# Root window settings
#######################################################

wm title . $Dict(Version) 
wm minsize . 300 20
wm maxsize . 300 20
. config -bg black

DivaMenuBar
DivaRestoreEnvironment

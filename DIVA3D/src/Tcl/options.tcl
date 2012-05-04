#######################################################
# p_options1 Procedure
#######################################################

proc p_options1 {} {

  global Coordinates Coordinates2 Dict

  CopArr Coordinates Coordinates2

  if { [winfo exist .options1] } { raise .options1
                                 return }

  toplevel .options1

  wm title .options1 $Dict(CoordinateSystem)
  wm geometry .options1   +400+2

#--------------------------------------------------------------
#  MakeFrame .options1 4 raised
  MakeFrame .options1 4 

  label .options1.1.label -text $Dict(CoordinateSystem) -width 30
  pack .options1.1.label

  radiobutton .options1.2.radio -text $Dict(Angular)  -relief flat \
    -variable Coordinates2(system) -value 1 \
    -command {DivaSwitchCoordinateSystem}
  radiobutton .options1.3.radio -text $Dict(Cartesian)  -relief flat \
    -variable Coordinates2(system) -value 0 \
    -command {DivaSwitchCoordinateSystem}
#  button .options1.3.label -text $Dict(Latitude) -relief flat
#  entry .options1.3.entry -relief sunken -textvariable Coordinates2(lambda) -width 3
#  radiobutton .options1.4.radio -text $Dict(Linear) -relief flat \
#    -variable Coordinates2(system) -value 2 \
#    -command {DivaSwitchCoordinateSystem}
  
#  pack .options1.2.radio .options1.3.radio .options1.4.radio -side left
   pack .options1.2.radio .options1.3.radio -side left
#  pack .options1.3.label .options1.3.entry  -padx 5 -pady 5 -anchor ne

#--------------------------------------------------------------

  button .options1.4.ok -text $Dict(Ok) -width 16 -command {
   CopArr Coordinates2 Coordinates
   QuitMenu .options 1 
  }
  button .options1.4.cancel -text $Dict(Cancel) -width 16 -command {
   QuitMenu .options 1 
  }

#  pack .options1.4.ok -side left -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
#  pack .options1.4.cancel -side right -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
  pack .options1.4.ok -side left 
  pack .options1.4.cancel -side right 

  DivaSwitchCoordinateSystem
}

#--------------------------------------------------------------
proc DivaSwitchCoordinateSystem {} {

  global Coordinates Coordinates2 disable_foreground active_foreground

  if { $Coordinates2(system) == 1 } {
    .options1.2.radio select
  } elseif { $Coordinates2(system) == 0 } {
    .options1.3.radio select
  } else {
   puts "bad coord sys. !"
  }

}

#######################################################
# p_options2 Procedure
#######################################################

proc p_options2 {} {

  global Dir Dir2  Dict 

  if { [winfo exist .options2] } { raise .options2 
                                 return }

  CopArr Dir Dir2

  toplevel .options2 

  wm title .options2 $Dict(DefaultDirectories) 
  wm geometry .options2 +440+2
  MakeFrame .options2 3

#--------------------------------------------------------------

  label .options2.1.label -text $Dict(DefaultDirectories) -width 70

  pack .options2.1.label

#--------------------------------------------------------------
  MakeFrame .options2.2 11 flat

  button .options2.2.1.button -text $Dict(Load) -padx 5 -command {LoadDefaultParentDir}
  label .options2.2.1.label -text $Dict(DefaultParentDir)
  entry .options2.2.1.entry -relief sunken -textvariable Dir2(work) -width 60

  label .options2.2.2.label -text $Dict(DirDataBase)
  entry .options2.2.2.entry -relief sunken -textvariable Dir2(database) -width 60
  label .options2.2.3.label -text $Dict(DirData)
  entry .options2.2.3.entry -relief sunken -textvariable Dir2(data) -width 60
  label .options2.2.4.label -text $Dict(DirMesh) 
  entry .options2.2.4.entry -relief sunken -textvariable Dir2(mesh) -width 60
  label .options2.2.5.label -text $Dict(DirAnalysis)
  entry .options2.2.5.entry -relief sunken -textvariable Dir2(analysis) -width 60
  label .options2.2.6.label -text $Dict(DirDBDescription)
  entry .options2.2.6.entry -relief sunken -textvariable Dir2(description) -width 60
  label .options2.2.7.label -text $Dict(DirBathymetry) 
  entry .options2.2.7.entry -relief sunken -textvariable Dir2(bathymetry) -width 60
  label .options2.2.8.label -text $Dict(DirContour)
  entry .options2.2.8.entry -relief sunken -textvariable Dir2(contour) -width 60
  label .options2.2.9.label -text $Dict(DirDepth) 
  entry .options2.2.9.entry -relief sunken -textvariable Dir2(depth) -width 60
  label .options2.2.10.label -text $Dict(DirTmp) 
  entry .options2.2.10.entry -relief sunken -textvariable Dir2(temp) -width 60
  label .options2.2.11.label -text $Dict(DirConfigFiles)
  entry .options2.2.11.entry -relief sunken -textvariable Dir2(options) -width 60

  pack .options2.2.1.button -side right
  pack .options2.2.1.label -side left
  pack .options2.2.1.entry -side right

  pack .options2.2.2.label -side left 
  pack .options2.2.2.entry -side right
  pack .options2.2.3.label -side left
  pack .options2.2.3.entry -side right 
  pack .options2.2.4.label -side left 
  pack .options2.2.4.entry -side right 
  pack .options2.2.5.label -side left 
  pack .options2.2.5.entry -side right 
  pack .options2.2.6.label -side left 
  pack .options2.2.6.entry -side right 
  pack .options2.2.7.label -side left 
  pack .options2.2.7.entry -side right 
  pack .options2.2.8.label -side left 
  pack .options2.2.8.entry -side right 
  pack .options2.2.9.label -side left 
  pack .options2.2.9.entry -side right 
  pack .options2.2.10.label -side left 
  pack .options2.2.10.entry -side right 
  pack .options2.2.11.label -side left 
  pack .options2.2.11.entry -side right 

  bind .options2.2.1.entry <Return> {Dir_SetDefaultParentDirectory}

# .options2.2.1.entry insert 0 "./"
#--------------------------------------------------------------

  MakeFrame .options2.3 2 flat

  button .options2.3.1.loaddescr -text $Dict(LoadConf) -width 16 -command { DirLoadConfigFile}
  button .options2.3.1.savedescr -text $Dict(SaveConf) -width 16 -command { DirSaveConfigFile}

  button .options2.3.2.ok -text $Dict(Ok) -width 16 -command {
   CopArr Dir2 Dir
   QuitMenu .options 2
  }
  button .options2.3.2.cancel -text $Dict(Cancel) -width 16 -command {
    QuitMenu .options 2
  }

  pack .options2.3.1.loaddescr .options2.3.2.ok -side left -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
  pack .options2.3.1.savedescr .options2.3.2.cancel -side right -ipadx 5 -padx 5 -pady 5 -fill x -expand 1

}

#######################################################

proc DirLoadConfigFile {} {
  global Fs Dir2
  LoadConfigFile Dir2 Dir2(VarList) Dir2(options) $Fs(dir_conf_filter)
}

#######################################################

proc DirSaveConfigFile {} {
  global Fs Dir2
  SaveConfigFile Dir2 Dir2(VarList) Dir2(options) $Fs(dir_conf_filter)
}

#######################################################

proc LoadDefaultParentDir {} {

  global Fs Dir2 Dict

  #DivaFileSelect .fs -full 0 -title $Dict(DefaultParentDir) -dir $Dir2(work) 
  #
  #if {[.fs activate]} {
  #   set  Dir2(work) [.fs get]
  #   Dir_SetDefaultParentDirectory
  #}
  #.fs delete
  #return
}

#######################################################

proc Dir_SetDefaultParentDirectory {} {

    global Dir2

    set Dir2(mesh) $Dir2(work)/MESH
    set Dir2(data) $Dir2(work)/DATA
    set Dir2(analysis) $Dir2(work)/ANALYSIS
    set Dir2(temp) $Dir2(work)/TMP
    set Dir2(database) $Dir2(work)/BASE
    set Dir2(options) $Dir2(work)/CONFIG
    set Dir2(description) $Dir2(work)/DES
    set Dir2(bathymetry) $Dir2(work)/BAT
    set Dir2(contour) $Dir2(work)/CONT
    set Dir2(depth) $Dir2(work)/DEPTH

}

#######################################################
# p_options3 Procedure
#######################################################

proc p_options3 {} {

  global Preference Preference2 Languages Dict language

  if { [winfo exist .options3] } { raise .options3
                                 return }

  CopArr Preference Preference2 

  toplevel .options3

  wm title .options3 $Dict(DefaultLanguage) 
  wm geometry .options3 +440+2
  wm minsize .options3 250 20
  MakeFrame .options3 3

#--------------------------------------------------------------

  label .options3.1.label -text $Dict(DefaultLanguage) 

  pack .options3.1.label

#--------------------------------------------------------------

  listbox .options3.2.list -relief raised -width 15 -height 5 -yscroll ".options3.2.scroll set" 
  scrollbar .options3.2.scroll -relief raised -command ".options3.2.list yview"
  bind .options3.2.list <B1-Motion> {%W select from [%W nearest %y]}

  pack .options3.2.scroll -side right  -fill y -pady 10 -padx 5
  pack .options3.2.list -side right -fill x -fill y -pady 10 -padx 5

  eval {.options3.2.list insert 0} $Languages
  set index [ lsearch $Languages $Preference2(language) ]
#  .options3.2.list select adjust [ expr $index ]

#--------------------------------------------------------------

  MakeFrame .options3.3 2 flat

  button .options3.3.ok -text $Dict(Ok) -width 5 -command {
   if { [.options3.2.list curselection ] >= 0 } {
    CopArr Preference2 Preference
    SwitchLanguage
   }
   QuitMenu .options 3
  }
  button .options3.3.cancel -text $Dict(Cancel) -width 5 -command {
   QuitMenu .options 3
  }

  pack .options3.3.ok -side left -ipadx 5 -padx 10 -pady 10 -fill x -expand 1
  pack .options3.3.cancel -side right -ipadx 5 -padx 10 -pady 10 -fill x -expand 1

  
}

#######################################################
#  SwitchLanguage
#######################################################
proc SwitchLanguage {} {
    global Dir Preference Languages env 
   
    if { $Preference(language) != \
	 [lindex $Languages [.options3.2.list curselection]] } {
      set Preference(language) \
	  [lindex $Languages [.options3.2.list curselection]]
      source $env(DIVA_SRC_DIR)/dictionary.tcl
      
      if { [winfo exist .data1] } {
       destroy .data1
       p_data1
      }
      if { [winfo exist .fem1] } {
       destroy .fem1
       p_fem1
      }
      if { [winfo exist .diva] } {
       destroy .diva 
       p_diva
      }
      if { [winfo exist .view1] } {
       destroy .view1
       p_view1
      }
      if { [winfo exist .view2] } {
       destroy .view2
       p_view2
      }
      if { [winfo exist .view3] } {
       destroy .view3
       p_view3
      }
      if { [winfo exist .options1] } {
       destroy .options1
       p_options1
      }
      if { [winfo exist .options2] } {
       destroy .options2
       p_options2
      }
      destroy .menubar
      DivaMenuBar
    }

}

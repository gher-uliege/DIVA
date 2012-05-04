#######################################################
# p_view1 procedure 
#######################################################

proc p_view1 { {w .view1} } {

  global Data GrData GrData2 Dict

  if { [winfo exist .view1] } { wm deiconify .view1
				raise .view1
                                return }

  CopArr GrData GrData2

  if {![info exists GrData2(depthlist)]} {
    set GrData2(depthlist) {}
  }

  VarEqu GrData name Data OutGenName .depth

  VarEqu GrData ContDescrName Data ContDescrName

  toplevel .view1 -bd 5 -relief ridge
  wm title .view1 $Dict(DataVisualisation)
  wm geometry .view1 350x425+200+50
  MakeFrame .view1 6

#--------------------------------------------------------------

  label .view1.1.label -text $Dict(DataVisualisation) -width 50
  pack .view1.1.label

#--------------------------------------------------------------
  radiobutton .view1.2.radio1 -text $Dict(2D) -relief flat \
    -variable GrData2(dim) -value 2 -command {Gr_Data_Switch2D3D}
  radiobutton .view1.2.radio2 -text $Dict(3D) -relief flat \
    -variable GrData2(dim) -value 3 -command {Gr_Data_Switch2D3D}

  pack .view1.2.radio1 -side left
  pack .view1.2.radio2 -side right

#--------------------------------------------------------------

  MakeFrame .view1.3 4 flat 0

  button .view1.3.1.label -text $Dict(DataFileName) -relief flat
  button .view1.3.1.load -text $Dict(Load) -padx 5 -command {Gr_Data_LoadDataFile}
  entry .view1.3.2.entry -relief sunken -textvariable GrData2(name)

  pack .view1.3.1.label -side left
  pack .view1.3.1.load -side right -padx 10 -pady 5
  pack .view1.3.2.entry -side left -fill x -expand 1 -pady 5

  button .view1.3.3.label -text $Dict(DepthFileName) -relief flat
  button .view1.3.3.load -text $Dict(Load) -padx 5 -command {Gr_Data_LoadDepthFile}
  entry .view1.3.4.entry -relief sunken -textvariable GrData2(DepthName)

  pack .view1.3.3.label -side left
  pack .view1.3.3.load -side right -padx 10 -pady 5
  pack .view1.3.4.entry -side left -fill x -expand 1 -pady 5

#--------------------------------------------------------------

  MakeFrame .view1.4 2 flat 0

  checkbutton .view1.4.1.chk -variable GrData2(ContDescr) -onvalue 1 \
    -offvalue 0 -text $Dict(DisplayContour) -command {Gr_Data_DisplayContCheck}
  button .view1.4.1.load -text $Dict(Load) -padx 5 -command {Gr_Data_LoadContFile}
  entry .view1.4.2.entry -relief sunken -textvariable GrData2(ContDescrName)  

  pack .view1.4.1.chk -side left -padx 5
  pack .view1.4.1.load -side right -padx 10 -pady 5
  pack .view1.4.2.entry -side left -fill x -expand 1 -pady 5

#--------------------------------------------------------------

  MakeFrame .view1.5 4 flat 0

  checkbutton .view1.5.1.chk -text $Dict(SelectedDepth) -variable GrData2(seldepth)\
    -onvalue 1 -offvalue 0

  label .view1.5.2.label -text $Dict(Title) 
  entry .view1.5.2.entry -relief sunken -width 30 -textvariable GrData2(title)

  label .view1.5.3.label -text $Dict(X_axis_name) 
  entry .view1.5.3.entry -relief sunken -width 30 -textvariable GrData2(xname)

  label .view1.5.4.label -text $Dict(Y_axis_name) 
  entry .view1.5.4.entry -relief sunken -width 30 -textvariable GrData2(yname)

  pack .view1.5.1.chk -side left -padx 5 -pady 5
  pack .view1.5.2.label -side left
  pack .view1.5.2.entry -side right
  pack .view1.5.3.label -side left
  pack .view1.5.3.entry -side right
  pack .view1.5.4.label -side left
  pack .view1.5.4.entry -side right

#--------------------------------------------------------------
  MakeFrame .view1.6 2 flat

  button .view1.6.1.loaddescr -text $Dict(LoadConf) -width 16 -command {DataViewLoadConfigFile}
  button .view1.6.1.savedescr -text $Dict(SaveConf) -width 16 -command {DataViewSaveConfigFile}
  button .view1.6.2.ok -text $Dict(Ok) -width 16 -command {
    CopArr GrData2 GrData
    Gr_DataVisu
  } 
  button .view1.6.2.cancel -text $Dict(Cancel) -width 16 -command {
    QuitMenu .view 1
  }

  pack .view1.6.1.loaddescr .view1.6.2.ok -side left -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
  pack .view1.6.1.savedescr .view1.6.2.cancel -side right -ipadx 5 -padx 5 -pady 5 -fill x -expand 1

  Gr_Data_DisplayContCheck
  Gr_Data_Switch2D3D
}

##############################################################################

proc Gr_Data_Switch2D3D {} {

 global GrData2 disable_foreground active_foreground

 if {$GrData2(dim) == 3} {
   .view1.3.1.label configure -state disabled
   .view1.3.1.load configure -state disabled
   .view1.3.2.entry configure -foreground $disable_foreground
    bind .view1.3.2.entry <FocusIn> {focus .view1}
   .view1.3.3.label configure -state normal
   .view1.3.3.load configure -state normal
   .view1.3.4.entry configure -foreground $active_foreground
    bind .view1.3.4.entry <FocusIn> {focus .view1.3.4.entry}
 } else {
   .view1.3.1.label configure -state normal
   .view1.3.1.load configure -state normal
   .view1.3.2.entry configure -foreground $active_foreground
    bind .view1.3.2.entry <FocusIn> {focus .view1.3.2.entry}
   .view1.3.3.label configure -state disabled
   .view1.3.3.load configure -state disabled
   .view1.3.4.entry configure -foreground $disable_foreground
    bind .view1.3.4.entry <FocusIn> {focus .view1}
 }

}

##############################################################################

proc Gr_Data_DisplayContCheck {} {

 global GrData2 disable_foreground active_foreground

 if {$GrData2(ContDescr) == 0} {
   .view1.4.1.load configure -state disabled
   .view1.4.2.entry configure -foreground $disable_foreground
    bind .view1.4.2.entry <FocusIn> {focus .view1}
 } else {
   .view1.4.1.load configure -state normal
   .view1.4.2.entry configure -foreground $active_foreground
    bind .view1.4.2.entry <FocusIn> {focus .view1.4.2.entry}
 }

}

##############################################################################

proc DataViewLoadConfigFile {} {

  global GrData2 Fs Dir

  LoadConfigFile  GrData2 GrData2(VarList) Dir(options) $Fs(gr_data_conf_filter)
  Gr_Data_Switch2D3D
}

##############################################################################

proc DataViewSaveConfigFile {} {

  global GrData2 Fs Dir

  SaveConfigFile GrData2 GrData2(VarList) Dir(options) $Fs(gr_data_conf_filter)
}

#######################################################
# p_view2 procedure 
#######################################################

proc p_view2 { {w .view2} } {

  global Fem GrFem GrFem2 Dict

  if { [winfo exist .view2] } { wm deiconify .view2
				raise .view2
                                return }

  CopArr GrFem GrFem2

  VarEqu GrFem femdim Fem dim
  VarEqu GrFem name Fem meshname
  if {[info exists Fem2(dim)]} {
    if {$Fem2(dim) == 3} {
      VarEqu GrFem2 depthname Fem depthname
    }
  }
  
  toplevel .view2 -bd 5 -relief ridge 
  wm title .view2 $Dict(MeshVisualisation) 
  wm geometry .view2 +300+50
  MakeFrame .view2 6

#--------------------------------------------------------------
# nouvelle fenêtre "Mesh view"
#----------------------------
  label .view2.1.label -text $Dict(MeshVisualisation) -width 50
  pack .view2.1.label 

#--------------------------------------------------------------
# choix cas 2D ou 3D
#-------------------

  radiobutton .view2.2.radio1 -text $Dict(2D) -relief flat \
    -variable GrFem2(femdim) -value 2 -command {GrFemSwitch2D3D}
  radiobutton .view2.2.radio2 -text $Dict(3D) -relief flat \
    -variable GrFem2(femdim) -value 3 -command {GrFemSwitch2D3D}

  pack .view2.2.radio1 -side left
  pack .view2.2.radio2 -side right

#--------------------------------------------------------------


  MakeFrame .view2.3 4 flat 0

# Choice of the mesh file  "Mesh file name"
# ---------------------------------------------

  label .view2.3.1.label -text $Dict(GenericMeshOuputFileName) 
  button .view2.3.1.load -text $Dict(Load) -padx 5 -command {Gr_Mesh_LoadMeshFile}
  entry .view2.3.2.entry -relief sunken -textvariable GrFem2(name) 
  pack .view2.3.1.label -side left
  pack .view2.3.1.load -side right -padx 10 
  pack .view2.3.2.entry -side left -fill x -expand 1 -pady 5

# Choice of the depth file "Depth file name"
# ------------------------------------------------

  button .view2.3.3.label -text $Dict(DepthFileName) -relief flat
  button .view2.3.3.load -text $Dict(Load) -padx 5 -command {Gr_Fem_LoadDepth}
  entry .view2.3.4.entry -relief sunken -textvariable GrData2(depthname) 
  pack .view2.3.3.label -side left
  pack .view2.3.3.load -side right -padx 10
  pack .view2.3.4.entry -side left -fill x -expand 1 -pady 5

# Title, x-axis, y-axis
#--------------------------------------------------------------

  MakeFrame .view2.4 4 flat 0

  checkbutton .view2.4.1.chk -text $Dict(SelectedDepth) -variable GrFem2(seldepth) \
    -onvalue 1 -offvalue 0

  label .view2.4.2.label -text $Dict(Title) 
  entry .view2.4.2.entry -relief sunken -width 30 -textvariable GrFem2(title)
 
  label .view2.4.3.label -text $Dict(X_axis_name) 
  entry .view2.4.3.entry -relief sunken -width 30 -textvariable GrFem2(xname)
 
  label .view2.4.4.label -text $Dict(Y_axis_name) 
  entry .view2.4.4.entry -relief sunken -width 30 -textvariable GrFem2(yname)
 
  pack .view2.4.1.chk -side left -padx 5 -pady 5
  pack .view2.4.2.label -side left
  pack .view2.4.2.entry -side right
  pack .view2.4.3.label -side left
  pack .view2.4.3.entry -side right
  pack .view2.4.4.label -side left
  pack .view2.4.4.entry -side right
 
#--------------------------------------------------------------

# label .view2.4.label -text "Aspect Ratio (-1 if unknown) :"
# entry .view2.4.entry -relief sunken -width 10 -textvariable GrFem2(asprt)

# pack .view2.4.label -side left
# pack .view2.4.entry -side right

#--------------------------------------------------------------

  MakeFrame .view2.5 2 flat

  button .view2.5.1.loaddescr -text $Dict(LoadConf) -width 16 -command { MeshViewLoadConfigFile}
  button .view2.5.1.savedescr -text $Dict(SaveConf) -width 16 -command { MeshViewSaveConfigFile}
  button .view2.5.2.ok -text $Dict(Ok) -width 16 -command {
   CopArr GrFem2 GrFem
   Gr_MeshVisu 
  } 
  button .view2.5.2.cancel -text $Dict(Cancel) -width 16 -command {
    QuitMenu .view 2
  }

  pack .view2.5.1.loaddescr .view2.5.2.ok -side left -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
  pack .view2.5.1.savedescr .view2.5.2.cancel -side right -ipadx 5 -padx 5 -pady 5 -fill x -expand 1

  GrFemSwitch2D3D
}

##############################################################################

proc GrFemSwitch2D3D {} {

 global GrFem2 disable_foreground active_foreground

 if {$GrFem2(femdim) == 2} {
  .view2.3.3.label configure -state disabled
  .view2.3.3.load  configure -state disabled
  .view2.3.4.entry configure -foreground $disable_foreground
   bind .view2.3.4.entry <FocusIn> {focus .view2}
 } else {
  .view2.3.3.label configure -state normal
  .view2.3.3.load  configure -state normal
  .view2.3.4.entry configure -foreground $active_foreground 
   bind .view2.3.4.entry <FocusIn> {focus .view2.3.4.entry}
 }

}

##############################################################################

proc MeshViewLoadConfigFile {} {

  global GrFem2 Fs Dir

  LoadConfigFile  GrFem2 GrFem2(VarList) Dir(options) $Fs(gr_mesh_conf_filter)
  GrFemSwitch2D3D
}

##############################################################################

proc MeshViewSaveConfigFile {} {

  global GrFem2 Fs Dir

  SaveConfigFile GrFem2 GrFem2(VarList) Dir(options) $Fs(gr_mesh_conf_filter)
}

#######################################################
# p_view3 procedure
#######################################################

proc p_view3 { {w .view3} } {

  global Diva GrDiva GrDiva2 Dict

  if { [winfo exist .view3] } { raise .view3
				wm deiconify .view3
                                return }

  CopArr GrDiva GrDiva2

  VarEqu GrDiva resname Diva OutName

  toplevel .view3 -bd 5 -relief ridge
  wm title .view3 $Dict(GridVisualisation) 
  wm geometry .view3 +400+50

  MakeFrame .view3 6

#--------------------------------------------------------------

  label .view3.1.label -text $Dict(GridVisualisation) -width 50
  pack .view3.1.label 

#--------------------------------------------------------------

  MakeFrame .view3.2 4 flat 0

  label .view3.2.1.label -text $Dict(AnalysisFileName)
  button .view3.2.1.load -text $Dict(Load) -padx 5 -command {Gr_LoadDivaFile} 
  entry .view3.2.2.entry -relief sunken -textvariable GrData2(resname)

  pack .view3.2.1.label -side left 
  pack .view3.2.1.load -side right -padx 10 -pady 5
  pack .view3.2.2.entry -side left -fill x -expand 1 -pady 5

#--------------------------------------------------------------

  MakeFrame .view3.3 4 flat 0

  checkbutton .view3.3.1.chk1 -text $Dict(SelectedDepthOrLevel)\
    -variable GrDiva2(seldepth) -onvalue 1 -offvalue 0 
  checkbutton .view3.3.1.chk2 -text $Dict(DisplayDepthOrLevel)\
    -variable GrDiva2(dispdepth) -onvalue 1 -offvalue 0 

  label .view3.3.2.label -text $Dict(Title) 
  entry .view3.3.2.entry -relief sunken -width 30 -textvariable GrDiva2(title)

  label .view3.3.3.label -text $Dict(X_axis_name) 
  entry .view3.3.3.entry -relief sunken -width 30 -textvariable GrDiva2(xname)

  label .view3.3.4.label -text $Dict(Y_axis_name) 
  entry .view3.3.4.entry -relief sunken -width 30 -textvariable GrDiva2(yname)

  pack .view3.3.1.chk1 -side left -pady 5 -padx 5
  pack .view3.3.1.chk2 -side right -pady 5 -padx 5 

  pack .view3.3.2.label -side left 
  pack .view3.3.2.entry -side right

  pack .view3.3.3.label -side left
  pack .view3.3.3.entry -side right

  pack .view3.3.4.label -side left
  pack .view3.3.4.entry -side right

#--------------------------------------------------------------

# label .view3.4.label -text "Aspect Ratio (-1 if unknown) :"
# entry .view3.4.entry -relief sunken -width 10 -textvariable GrDiva2(asprt)

# pack .view3.4.label -side left
# pack .view3.4.entry -side right

#--------------------------------------------------------------

  MakeFrame .view3.5 2 flat 0

  label .view3.5.1.label -text $Dict(ColorLevels) 
  entry .view3.5.1.entry -relief sunken -width 10 -textvariable GrDiva2(clrlv)
  checkbutton .view3.5.2.chk -relief flat \
    -text $Dict(ComputeColorsAtAllDepths) -variable GrDiva2(depthscale) \
    -onvalue "1." -offvalue "0."

  pack .view3.5.1.label -side left
  pack .view3.5.1.entry -side right
  pack .view3.5.2.chk -side left
  
#--------------------------------------------------------------
  MakeFrame .view3.6 2 flat

  button .view3.6.1.loaddescr -text $Dict(LoadConf) -width 16 -command { DivaViewLoadConfigFile}
  button .view3.6.1.savedescr -text $Dict(SaveConf) -width 16 -command { DivaViewSaveConfigFile}
  button .view3.6.2.ok -text $Dict(Ok) -width 16 -command {
   CopArr GrDiva2 GrDiva
   Gr_DivaVisu
  } 
  button .view3.6.2.cancel -text $Dict(Cancel) -width 16 -command {
   QuitMenu .view 3
  }

  pack .view3.6.1.loaddescr .view3.6.2.ok -side left -ipadx 5 -padx 5 -pady 5 -fill x -expand 1
  pack .view3.6.1.savedescr .view3.6.2.cancel -side right -ipadx 5 -padx 5 -pady 5 -fill x -expand 1

}

##############################################################################

proc DivaViewLoadConfigFile {} {

  global GrDiva2 Fs Dir

  LoadConfigFile  GrDiva2 GrDiva2(VarList) Dir(options) $Fs(gr_diva_conf_filter)
}

##############################################################################

proc DivaViewSaveConfigFile {} {

  global GrDiva2 Fs Dir

  SaveConfigFile GrDiva2 GrDiva2(VarList) Dir(options) $Fs(gr_diva_conf_filter)
}

##############################################################################

proc Gr_DataVisu {} {

  global GrData PlplotDev Gra Dict

  if {$GrData(dim) == 2} {
   if {![info exists GrData(name)] || ![file exists $GrData(name)]} {
    ErrorMessageBox Error $Dict(BadDataFile)
    return
   }
  } else {
   if {![info exists GrData(DepthName)] || ![file exists $GrData(DepthName)]} {
    ErrorMessageBox Error $Dict(BadDepthFile)
    return
   }
  }

  set GrData(datanamelist) $GrData(name)

  if {$GrData(dim) == 3} {

#    LoadDepthList GrData(name) GrData(depthlist) GrData(datanamelist) 
    LoadDepthList GrData(DepthName) GrData(depthlist) GrData(datanamelist)

    if {$GrData(seldepth)} {
      set ind [SelectDepth GrData(depthlist)]
      if {$ind != -1} {
        set GrData(depthlist) [lindex $GrData(depthlist) $ind]
        set GrData(datanamelist) [lindex $GrData(datanamelist) $ind]
      } else {
        return
      }
    } 
  }

  set nbdatafile [llength $GrData(datanamelist)]

  set fvisuid [open |$Gra(visu_exec) a]
# set fvisuid "stdout" 

  puts $fvisuid "0"
  puts $fvisuid $nbdatafile
  if {$GrData(dim) == 3} {
    for {set i 0} {$i < $nbdatafile} {incr i 1} {
      set pr [lsort [lindex $GrData(depthlist) $i]]
      set dt [lindex $GrData(datanamelist) $i]
      set nbd [exec wc $dt]
      set nbdata [FirstElemStr $nbd]
#     set nbdata [lsort [exec wc $dt | cut -c1-8]]
#      puts $fvisuid "$GrData(title) $pr meters, $nbdata data"
      puts $fvisuid "$GrData(title) $pr, $nbdata data"
      puts $fvisuid $dt
    }
  } else {
    set nbd [exec wc $GrData(name)]
    set nbdata [FirstElemStr $nbd]
#   set nbdata [lsort [exec wc $GrData(name) | cut -c1-8]]
    puts $fvisuid "$GrData(title) $nbdata data"
    puts $fvisuid $GrData(datanamelist)
  }
  if {$GrData(ContDescr)} {
    puts $fvisuid $GrData(ContDescrName)
  } else {
    puts $fvisuid "0"
  }
# puts $fvisuid $GrData(title)
  puts $fvisuid $GrData(xname)
  puts $fvisuid $GrData(yname)
  puts $fvisuid $PlplotDev

  flush $fvisuid
# close $fvisuid
  

}

##############################################################################

#proc Gr_Data_LoadDepthFile {} {

#  global GrData2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DepthFileName) \
#    -full 0 -dir $Dir(depth) -filter $Fs(gr_data_name_depth_filter)

#  if {[.fs activate]} {
#    set GrData2(DepthName) [.fs get]
#  }
#  .fs delete
#  return

#}

proc Gr_Data_LoadDepthFile {} {

global GrData2 Fs Dir Dict

set GrData2(DepthName) [tk_getOpenFile\
  -defaultextension .depth\
  -initialdir $Dir(depth) \
  -initialfile {adriatic.data.depth}\
  -title $Dict(DepthFileName)
]
return
}

##############################################################################

#proc Gr_Data_LoadDataFile {} {
#
#  global GrData2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DataFileName) \
#    -full 0 -dir $Dir(data) -filter $Fs(gr_data_name_filter)
#
#  if {[.fs activate]} {
#    set GrData2(name) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Gr_Data_LoadDataFile {} {

global GrData2 Fs Dir Dict

set GrData2(name) [tk_getOpenFile\
  -defaultextension .data\
  -initialdir $Dir(data)\
  -initialfile {adriatic.data.01}\
  -title $Dict(DataFileName)
]
return
}

##############################################################################

#proc Gr_Data_LoadContFile {} {
#
# global GrData2 Fs Dir Dict
#
# DivaFileSelect .fs -title $Dict(LoadContourDescriptionFile) \
#   -full 0 -dir $Dir(contour) -filter $Fs(gr_data_cont_filter)
#
# if {[.fs activate]} {
#   set GrData2(ContDescrName) [.fs get]
# }
# .fs delete
# return
#
#

proc Gr_Data_LoadContFile {} {

global GrData2 Fs Dir Dict

set GrData2(ContDescrName) [tk_getOpenFile\
  -defaultextension .cont\
  -initialdir $Dir(contour)\
  -initialfile {adriatic.cont}\
  -title $Dict(LoadContourDescriptionFile)
]
return
}

##############################################################################

proc Gr_MeshVisu {} {

  global GrFem PlplotDev Gra Dict

  set GrFem(depthlist) {}

  if {$GrFem(femdim) == 3} {

    if {![info exists GrFem(depthname)] || ![file exists $GrFem(depthname)]} {
      ErrorMessageBox Error $Dict(BadDepthFile)
      return
    }

    LoadDepthList GrFem(depthname) GrFem(depthlist)

    if {$GrFem(seldepth)} {
      set ind [SelectDepth GrFem(depthlist)]
      if {$ind != -1} {
        set GrFem(depthlist) [lindex $GrFem(depthlist) $ind]
      } else {
        return
      }
    }
  }

  set nbprof [llength $GrFem(depthlist)]
 
  set fvisuid [open |$Gra(visu_exec) a]
# set fvisuid stdout
  puts $fvisuid "1"
 
  if { [CutString last $GrFem(name) 3] != ".xy" } {
   puts $fvisuid "$GrFem(name).mh4"
   puts $fvisuid "$GrFem(name).mh5"
   puts $fvisuid "$GrFem(name)"
  } else {
    set str [CutString first $GrFem(name) 3]
    puts $fvisuid "$str.mh4"
    puts $fvisuid "$str.mh5"
    puts $fvisuid "$GrFem(name)"
  }
  puts $fvisuid "$nbprof"
  if {$GrFem(femdim) == 3} {
    if {$GrFem(seldepth)} {
      puts $fvisuid [expr $ind+1]
      set pr [lsort $GrFem(depthlist)]
#      puts $fvisuid "$GrFem(title) $pr meters"
      puts $fvisuid "$GrFem(title) $pr"
    } else { 
      for {set i 0} {$i < $nbprof} {incr i 1} {
        puts $fvisuid [expr $i+1]
        set pr [lsort [lindex $GrFem(depthlist) $i]]
        puts $fvisuid "$GrFem(title) $pr"
      }
    }
  } else {
    puts $fvisuid "$GrFem(title)"
  }
  puts $fvisuid "$GrFem(xname)"
  puts $fvisuid "$GrFem(yname)"
# puts $fvisuid "$GrFem(asprt)"
  puts $fvisuid "$PlplotDev"
  flush $fvisuid 
# close $fvisuid

}

##############################################################################

#proc Gr_Fem_LoadDepth {} {
#
#  global GrFem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DepthFileName) \
#    -filter $Fs(gr_mesh_depth_filter) -full 0 -dir $Dir(depth)
#
#  if {[.fs activate]} {
#    set GrFem2(depthname) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Gr_Fem_LoadDepth {} {

global GrData2 Fs Dir Dict

set GrData2(depthname) [tk_getOpenFile\
  -defaultextension .depth\
  -initialdir $Dir(depth)\
  -initialfile {adriatic.data.depth}\
  -title $Dict(DepthFileName)
]
return
}



##############################################################################


proc Gr_DivaVisu {} {

  global GrDiva PlplotDev Gra Dict

  if { [CutString last $GrDiva(resname) 3] == ".xy" } {
    set resname [CutString first $GrDiva(resname) 3]
  } else {
   set resname $GrDiva(resname)
  }
#  puts $resname

 if { ![file exists $GrDiva(resname)]} {

#  if {![info exists $GrDiva(resname)] || ![file exists $GrDiva(resname)]}
    ErrorMessageBox Error $Dict(BadAnalysisFile)
    return
  }

  set info 1
  if {![file exists $resname.info]} {set info 0} 

  if {$info} {
    set infoid [open $resname.info r]
    if [catch { gets $infoid lonmin
                gets $infoid lonmax
                gets $infoid latmin
                gets $infoid latmax
                gets $infoid nbdepth
                set nbdepth [lsort $nbdepth]
                if {$nbdepth > 0} {
                  set GrDiva(depthlist) {}
                  set GrDiva(titlelist) {}
                  for {set i 0} {$i < $nbdepth} {incr i 1} {
                    gets $infoid prof
                    set prof [lsort $prof]
                    lappend GrDiva(depthlist) $prof
                    if {$GrDiva(dispdepth)} {
                      lappend GrDiva(titlelist) "$GrDiva(title) $prof"
#                      lappend GrDiva(titlelist) "$GrDiva(title) $prof meters"
                    } else {
                      lappend GrDiva(titlelist) $GrDiva(title)
                    }
                  }
                  if {$nbdepth == 1} {
                    set level 1
                  } 
                } else {
                    set nbdepth 1
                    set level 1
                    set GrDiva(titlelist) {} 
                    lappend GrDiva(titlelist) $GrDiva(title)
                }
                close $infoid
              } errtmp] {
        ErrorMessageBox Error "Error reading $resname.info file !" 
        close $infoid
        return
    }
  } else {
    set headerid [open |$Gra(header_exec) r+]
    puts $headerid $resname
    flush $headerid
    gets $headerid imax
    gets $headerid jmax
    gets $headerid nbdepth
    close $headerid
    set nbdepth [lsort $nbdepth]
    set GrDiva(depthlist) {}
    set GrDiva(titlelist) {}
    for {set i 0} {$i < $nbdepth} {incr i 1} {
      set level [expr $nbdepth - $i]
      lappend GrDiva(depthlist) $level 
      if {$GrDiva(dispdepth)} {
        lappend GrDiva(titlelist) "$GrDiva(title) level $level" 
      } else {
        lappend GrDiva(titlelist) $GrDiva(title)
      }
    }
  }

  if {$GrDiva(seldepth) && $nbdepth > 1} {
    set ind [SelectDepth GrDiva(depthlist)]
    if {$ind != -1} {
      set titletmp [lindex $GrDiva(titlelist) $ind]
      set GrDiva(titlelist) {}
      lappend GrDiva(titlelist) $titletmp
      set level [expr $nbdepth - $ind] 
      set nbdepth 1
    } else {
      return
    }
  }

# set fvisuid stdout 
  set fvisuid [open |$Gra(visu_exec) a]
  puts $fvisuid "2"
  puts $fvisuid "$GrDiva(resname)"
  puts $fvisuid "$GrDiva(clrlv)"
  puts $fvisuid "$info"
  if {$info} {
    puts $fvisuid $lonmin
    puts $fvisuid $lonmax
    puts $fvisuid $latmin
    puts $fvisuid $latmax
  }
  puts $fvisuid $nbdepth
  if {$nbdepth == 1} {
    puts $fvisuid $level
    puts $fvisuid [lindex $GrDiva(titlelist) 0]
  } else {
    for {set i [expr $nbdepth-1]} {$i >= 0} {incr i -1} {
      puts $fvisuid [lindex $GrDiva(titlelist) $i]
    }
  }
  puts $fvisuid "$GrDiva(depthscale)"
  puts $fvisuid "$GrDiva(xname)"
  puts $fvisuid "$GrDiva(yname)"
# puts $fvisuid "$GrDiva(asprt)"
  puts $fvisuid "$PlplotDev"
  flush $fvisuid 
# close $fvisuid


}

##############################################################################
#proc Gr_Mesh_LoadMeshFile {} {
#
#  global GrFem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(MeshFileName) \
#    -filter $Fs(gr_mesh_name_filter) -full 0 -dir $Dir(mesh)
#
#  if {[.fs activate]} {
#    set  GrFem2(name) [.fs get]
#  }
#  .fs delete
#  return

#}

proc Gr_Mesh_LoadMeshFile {} {

global GrFem2 Fs Dir Dict

set GrFem2(name) [tk_getOpenFile\
  -defaultextension .mesh\
  -initialdir $Dir(mesh)\
  -initialfile {adriatic.mesh}\
  -title $Dict(MeshFileName)
]
return
}


##############################################################################

#proc Gr_LoadDivaFile {} {
#
#  global GrDiva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(AnalysisFileName) -full 0 \
#	-dir $Dir(analysis) -filter $Fs(gr_diva_name_res)
#
#  if {[.fs activate]} {
#    set GrDiva2(resname) [.fs get]
#  }
#  .fs delete
#  return
#} 

proc Gr_LoadDivaFile {} {

global GrData2 Fs Dir Dict

set GrData2(resname) [tk_getOpenFile\
  -defaultextension .anl\
  -initialdir $Dir(analysis)\
  -initialfile {adriatic.anl}\
  -title $Dict(AnalysisFileName)
]
return
}


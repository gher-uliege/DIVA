#######################################################
# p_data1 Procedure
#######################################################

proc p_data1 { } {

  global Data Data2 Fs Dict

  if { [winfo exist .data1] } { wm deiconify .data1
                                raise .data1
                                return }

  if {![info exists Data(depthlist)]} {
    set Data(depthlist) {}
  }

  CopArr Data Data2

  toplevel .data1 -bd 5 -relief ridge
  wm title .data1 $Dict(DataExtraction) 
  wm geometry .data1 +25+25

  MakeFrame .data1 10

#--------------------------------------------------------------

#  label .data1.1.label -text $Dict(DataExtraction) 
#  .data1.1.label config -width 50 
#  pack .data1.1.label -side top

#--------------------------------------------------------------

 # MakeFrame .data1.2 2 flat

 # label .data1.2.1.label -text $Dict(DBDescriptionFileName) 
 # button .data1.2.1.load -text $Dict(Load) -padx 5 -command {DescrDataFileSelect}
 # entry .data1.2.2.entry -relief sunken -textvariable Data2(DBDescrname)
 
 # pack .data1.2.1.label -side left
 # pack .data1.2.1.load -side right -padx 10
 # pack .data1.2.2.entry -side left -fill x -expand 1 -pady 5

  MakeFrame .data1.2 1 flat

  label .data1.2.1.label -text $Dict(DBDescriptionFileName) 
  entry .data1.2.1.entry -relief sunken -width 50 -textvariable Data2(DBDescrname)
  button .data1.2.1.load -text $Dict(Load) -padx 5 -command {DescrDataFileSelect}

  pack .data1.2.1.label -side left
  pack .data1.2.1.load -side right -padx 10
  pack .data1.2.1.entry -side right -padx 10


#--------------------------------------------------------------

 # MakeFrame .data1.3 2 flat
#
#  label .data1.3.1.label -text $Dict(GenericDataOutputFileName) 
#  button .data1.3.1.load -text $Dict(Load) -padx 5 -command {OutputFileSelect}
#  entry .data1.3.2.entry -relief sunken -textvariable Data2(OutGenName)
#
#  pack .data1.3.1.label -side left
#  pack .data1.3.1.load -side right -padx 10
#  pack .data1.3.2.entry -side left -fill x -expand 1 -pady 5
#
  MakeFrame .data1.3 1 flat

  label .data1.3.1.label -text $Dict(GenericDataOutputFileName) 
  entry .data1.3.1.entry -relief sunken -width 50 -textvariable Data2(OutGenName)
  button .data1.3.1.load -text $Dict(Load) -padx 5 -command {OutputFileSelect}

  pack .data1.3.1.label -side left
  pack .data1.3.1.load -side right -padx 10
  pack .data1.3.1.entry -side right -padx 10

#--------------------------------------------------------------

  frame .data1.4.1 -relief flat -bd 2
  pack .data1.4.1 -side left  -fill y
  frame .data1.4.2 -relief flat -bd 2
  pack .data1.4.2 -side right

  MakeFrame .data1.4.1  4 flat

  label .data1.4.1.1.label1 -text $Dict(Years) 
  label .data1.4.1.2.label1 -text $Dict(From) 
  entry .data1.4.1.2.entry1 -relief sunken -width 4 -textvariable Data2(yearmin)
  label .data1.4.1.2.label2 -text $Dict(To) 
  entry .data1.4.1.2.entry2 -relief sunken -width 4 -textvariable Data2(yearmax)

  label .data1.4.1.3.label1 -text $Dict(Period) 
  label .data1.4.1.4.label1 -text $Dict(From) 
  entry .data1.4.1.4.entry1 -relief sunken -width 4 -textvariable Data2(permin)
  label .data1.4.1.4.label2 -text $Dict(To) 
  entry .data1.4.1.4.entry2 -relief sunken -width 4 -textvariable Data2(permax)

  pack .data1.4.1.1.label1 -side left
  pack .data1.4.1.2.label1 -padx 15 -side left 
  pack .data1.4.1.2.entry1 .data1.4.1.2.label2 \
		.data1.4.1.2.entry2 -side left -padx 5
  pack .data1.4.1.3.label1 -side left
  pack .data1.4.1.4.label1 -padx 15 -side left
  pack .data1.4.1.4.entry1 .data1.4.1.4.label2 \
		.data1.4.1.4.entry2 -side left -padx 5 

  MakeFrame .data1.4.2 3 flat

  label .data1.4.2.3.label -text $Dict(Depth) -relief flat
  entry .data1.4.2.3.entry -relief sunken -width 6 -textvariable Data2(depth)

#  pack .data1.4.2.3.label .data1.4.2.1.entry -side right -fill x

  button .data1.4.2.3.add -text $Dict(Add) -bd 2 -command {Data_Add_Depth_List}
  button .data1.4.2.3.del -text $Dict(Del) -bd 2 -command {Data_Rem_Depth_List}

#  pack .data1.4.2.3.add -side left -padx 5 -pady 5 -ipadx 5
#  pack .data1.4.2.3.del -side right -padx 5 -pady 5 -ipadx 5

  listbox .data1.4.2.3.list -relief raised -width 10 -height 5\
    -yscroll ".data1.4.2.3.scroll set"
  scrollbar .data1.4.2.3.scroll -relief raised -command ".data1.4.2.3.list \
    yview"

  bind .data1.4.2.3.list <ButtonRelease-1> {Data_Depth_Get_List}
  bind .data1.4.2.3.list <B1-Motion> {%W select from [%W nearest %y]}
  bind .data1.4.2.3.list <Shift-1> {%W select from [%W nearest %y]}
  bind .data1.4.2.3.list <Shift-B1-Motion> {%W select from [%W nearest %y]}
  bind .data1.4.2.3.entry <Delete> {Data_Rem_Depth_List}
  bind .data1.4.2.3.entry <Return> {Data_Add_Depth_List}

  pack .data1.4.2.3.list -side left -fill y
  pack .data1.4.2.3.scroll -side left -fill y -anchor w
  pack .data1.4.2.3.label .data1.4.2.3.entry -anchor e -side top -padx 5 -pady 5 -ipadx 5 -fill x
  pack .data1.4.2.3.del .data1.4.2.3.add -anchor e -side bottom -padx 5 -pady 5 -ipadx 5 -fill x
  
  if {[info exists Data2(depthlist)]} {
    eval {.data1.4.2.3.list insert 0} $Data2(depthlist)
  }

#--------------------------------------------------------------

  frame .data1.5.00 -relief flat -bd 2
  frame .data1.5.0 -relief flat -bd 2

  pack .data1.5.00 -side top -anchor nw -expand true -fill x
  pack .data1.5.0 -side top -anchor nw -expand true -fill x

  radiobutton .data1.5.00.radio1 -variable Data2(ContDescr) -value 1 -command {DataSwitchSelection}
  button .data1.5.00.label -text $Dict(ContourDescriptionFile) -relief flat
  button .data1.5.00.load -text $Dict(Load) -padx 5 -command {ContourFileSelect}
  entry .data1.5.0.entry -relief sunken -textvariable Data2(ContDescrName)

  pack .data1.5.00.radio1 .data1.5.00.label -side left
  pack .data1.5.00.load -side right -padx 10
  pack .data1.5.0.entry -side left -fill x -expand 1 -pady 5

  frame .data1.5.1 -relief flat -bd 2 
  frame .data1.5.2 -relief flat -bd 2

  pack .data1.5.1 -side right -anchor ne -expand true
  pack .data1.5.2 -side left -anchor nw -expand true -fill y

  MakeFrame .data1.5.2 3 flat 

  radiobutton .data1.5.2.1.radio -variable Data2(ContDescr) -value 0 -command {DataSwitchSelection}
  button .data1.5.2.1.label -text $Dict(Position) -relief flat

  
  button .data1.5.2.2.label1 -text $Dict(MinimumLongitude) -relief flat -width 15 
  button .data1.5.2.2.label2 -text $Dict(MaximumLongitude) -relief flat -width 15 
  button .data1.5.2.3.label1 -text $Dict(MinimumLatitude) -relief flat  -width 15 
  button .data1.5.2.3.label2 -text $Dict(MaximumLatitude) -relief flat  -width 15 
 
  entry .data1.5.2.2.entry1 -relief sunken -width 10 -textvariable Data2(lonmin)
  entry .data1.5.2.2.entry2 -relief sunken -width 10 -textvariable Data2(lonmax)
  entry .data1.5.2.3.entry1 -relief sunken -width 10 -textvariable Data2(latmin)
  entry .data1.5.2.3.entry2 -relief sunken -width 10 -textvariable Data2(latmax)
   
  pack .data1.5.2.1.radio .data1.5.2.1.label -side left  
 
  #pack .data1.5.2.2.label1 -side left -pady 6 -padx 5
  #pack .data1.5.2.2.entry1 -side left -pady 6 -padx 5
  #pack .data1.5.2.2.entry2 -side right -pady 6 -padx 5
  #pack .data1.5.2.2.label2 -side right -pady 6 -padx 5
  #pack .data1.5.2.3.label1 -side left -pady 6 -padx 5
  #pack .data1.5.2.3.entry1 -side left -pady 6 -padx 5
  #pack .data1.5.2.3.entry2 -side right -pady 6 -padx 5
  #pack .data1.5.2.3.label2 -side right -pady 6 -padx 5

  pack .data1.5.2.2.label1 -side left -padx 5
  pack .data1.5.2.2.entry1 -side left -padx 5
  pack .data1.5.2.2.entry2 -side right  -padx 5
  pack .data1.5.2.2.label2 -side right  -padx 5
  pack .data1.5.2.3.label1 -side left   -padx 5
  pack .data1.5.2.3.entry1 -side left   -padx 5
  pack .data1.5.2.3.entry2 -side right  -padx 5
  pack .data1.5.2.3.label2 -side right   -padx 5 

#--------------------------------------------------------------

  MakeFrame .data1.6 1 flat

  label .data1.6.1.label -text $Dict(DataType) 
  checkbutton .data1.6.1.chk1 -relief flat -cursor arrow \
    -text $Dict(CTD)  -variable Data2(ctd) 
  checkbutton .data1.6.1.chk2 -relief flat -cursor arrow \
    -text $Dict(Bottle) -variable Data2(bottle) 
  checkbutton .data1.6.1.chk3 -relief flat -cursor arrow \
    -text $Dict(BT) -variable Data2(bt) 

  pack .data1.6.1.label -side left 
  pack .data1.6.1.chk3 .data1.6.1.chk2 .data1.6.1.chk1 -side right

#--------------------------------------------------------------

  MakeFrame .data1.7 1 flat

  label .data1.7.1.label -text $Dict(DataNature) 
  radiobutton .data1.7.1.radio1 -relief flat -text $Dict(Temperature) \
     -variable Data2(TS) -value 1 
  radiobutton .data1.7.1.radio2 -relief flat -text $Dict(Salinity) \
     -variable Data2(TS) -value 2 

  pack .data1.7.1.label -side left
  pack .data1.7.1.radio2 .data1.7.1.radio1 -side right  

#--------------------------------------------------------------

  MakeFrame .data1.8 3 flat

  label .data1.8.1.label -text $Dict(QualityCriterion)
  pack .data1.8.1.label

  label .data1.8.2.label1 -text $Dict(AcceptableProfileRange)
  label .data1.8.2.label2 -text $Dict(From) 
  entry .data1.8.2.entry1 -relief sunken -textvariable Data2(GlobalFlagMin) \
    -width 1
  label .data1.8.2.label3 -text $Dict(To) 
  entry .data1.8.2.entry2 -relief sunken -textvariable Data2(GlobalFlagMax) \
    -width 1

  pack .data1.8.2.label1 -side left
  pack .data1.8.2.entry2 .data1.8.2.label3 .data1.8.2.entry1 \
    .data1.8.2.label2 -side right -padx 5

  label .data1.8.3.label1 -text $Dict(AcceptableObservationRange)
  label .data1.8.3.label2 -text $Dict(From) 
  entry .data1.8.3.entry1 -relief sunken -textvariable Data2(FlagMin) -width 1
  label .data1.8.3.label3 -text $Dict(To) 
  entry .data1.8.3.entry2 -relief sunken -textvariable Data2(FlagMax) -width 1

  pack .data1.8.3.label1 -side left
  pack .data1.8.3.entry2 .data1.8.3.label3 .data1.8.3.entry1 \
    .data1.8.3.label2 -side right -padx 5

#--------------------------------------------------------------

  MakeFrame .data1.9 2 flat

  button .data1.9.1.loaddescr -text $Dict(LoadConf) -width 16 -command \
    {DataLoadConfigFile} 

  button .data1.9.1.savedescr -text $Dict(SaveConf) -width 16 -command \
    {DataSaveConfigFile}

  button .data1.9.2.extract -text "$Dict(Ok)" -width 16 -command {
    CopArr Data2 Data
    Data_Extract 
  }
  button .data1.9.2.cancel -text "$Dict(Cancel)" -width 16 -command {
    QuitMenu .data 1
  } 

  pack .data1.9.1.loaddescr .data1.9.1.savedescr -side left -fill x \
    -expand 1 -padx 5 -pady 5
  pack .data1.9.2.extract .data1.9.2.cancel -side left -fill x \
    -expand 1 -padx 5 -pady 5

  DataSwitchSelection


#--------------------------------------------------------------
   
  MakeFrame .data1.10 1 flat

  button .data1.10.1.open -text $Dict(VisuData) -relief raised\
  -command {VisuDataOutputFile}
  pack .data1.10.1.open -side top -expand 1 -fill x 
  

}

##############################################################################


proc DescrDataFileSelect {} {
global Data2 Fs Dir Dict

set Data2(DBDescrname) [
 tk_getOpenFile  -defaultextension .des\
 -initialdir $Dir(description)\
 -initialfile {liste.des}\
 -title $Dict(LoadDataBaseDescriptionFile)
]
return
}

##############################################################################

proc OutputFileSelect {} {
global Data2 Fs Dir Dict
set Data2(OutGenName) [
tk_getSaveFile -defaultextension .dat\
  -initialdir $Dir(data) \
  -initialfile {adriatic.dat}\
  -title $Dict(OutputFileGenericName)
]
return
}

###############################################################################

proc ContourFileSelect {} {
global Data2 Fs Dir Dict
set Data2(ContDescrName) [
  tk_getOpenFile -defaultextension .cont\
  -initialdir $Dir(contour) \
  -initialfile {adriatic.cont}\
  -title $Dict(LoadContourDescriptionFile)]
return
}

##############################################################################

proc DataSwitchSelection {} {

 global Data2 disable_foreground active_foreground

 if {$Data2(ContDescr) == 0} {
  .data1.5.00.label configure -state disabled
  .data1.5.00.load  configure -state disabled
  .data1.5.0.entry configure -foreground $disable_foreground
  .data1.5.2.1.label configure -state normal
  .data1.5.2.2.label1 configure -state normal
  .data1.5.2.2.label2 configure -state normal
  .data1.5.2.3.label1 configure -state normal
  .data1.5.2.3.label2 configure -state normal
  .data1.5.2.2.entry1 configure -foreground $active_foreground
  .data1.5.2.2.entry2 configure -foreground $active_foreground
  .data1.5.2.3.entry1 configure -foreground $active_foreground
  .data1.5.2.3.entry2 configure -foreground $active_foreground
  bind .data1.5.2.2.entry1 <FocusIn> {focus .data1.5.2.2.entry1}
  bind .data1.5.2.2.entry2 <FocusIn> {focus .data1.5.2.2.entry2} 
  bind .data1.5.2.3.entry1 <FocusIn> {focus .data1.5.2.3.entry1}
  bind .data1.5.2.3.entry2 <FocusIn> {focus .data1.5.2.3.entry2}
  bind .data1.5.0.entry <FocusIn> {focus .data1}
 } else {
  .data1.5.00.label configure -state normal
  .data1.5.00.load  configure -state normal
  .data1.5.0.entry configure -foreground $active_foreground
  .data1.5.2.1.label configure -state disabled
  .data1.5.2.2.label1 configure -state disabled
  .data1.5.2.2.label2 configure -state disabled
  .data1.5.2.3.label1 configure -state disabled
  .data1.5.2.3.label2 configure -state disabled
  .data1.5.2.2.entry1 configure -foreground $disable_foreground
  .data1.5.2.2.entry2 configure -foreground $disable_foreground
  .data1.5.2.3.entry1 configure -foreground $disable_foreground
  .data1.5.2.3.entry2 configure -foreground $disable_foreground
  bind .data1.5.2.2.entry1 <FocusIn> {focus .data1}
  bind .data1.5.2.2.entry2 <FocusIn> {focus .data1}
  bind .data1.5.2.3.entry1 <FocusIn> {focus .data1}
  bind .data1.5.2.3.entry2 <FocusIn> {focus .data1}
  bind .data1.5.0.entry <FocusIn> {focus .data1.5.0.entry}
 }

}


##############################################################################

proc Data_Add_Depth_List {} {

  global Data2 Dict

  set flag [regexp {^[0-9]+$} $Data2(depth)]

  if {!$flag} {
    ErrorMessageBox Error $Dict(DepthNotValid)
    return
  } else {

    set val [format "%5.0f" $Data2(depth)]
    set val [join $val ""]

    if {$val >= 20000} {
      ErrorMessageBox Error  $Dict(DepthInf20000)
      return
    } else {

      if {[lsearch $Data2(depthlist) $val] != -1} {
        ErrorMessageBox Error $Dict(DepthAlreadyExists)
        return
      } else {

        lappend Data2(depthlist) $val
        set Data2(depthlist) [lsort -integer $Data2(depthlist)]
        .data1.4.2.3.list delete 0 [expr [llength $Data2(depthlist)]-2]
        eval {.data1.4.2.3.list insert end} $Data2(depthlist)
        set Data2(depth) ""
      }
    }
  }
} 

##############################################################################

proc Data_Rem_Depth_List {} {

  global Data2

  if {$Data2(depthlist) == {} } {
    return
  } else {

    set flag [lsearch $Data2(depthlist) $Data2(depth)]
    if {$flag == -1} {
      return
    } else {

      set Data2(depthlist) [lreplace $Data2(depthlist) $flag $flag]
      .data1.4.2.3.list delete 0 [expr [llength $Data2(depthlist)]]
      eval {.data1.4.2.3.list insert end} $Data2(depthlist)
    }
  }

}

##############################################################################

proc Data_Depth_Get_List {} {

  global Data2

  if {[info exists Data(depthlist)]} {
    if {$Data2(depthlist) != {} } {
      set Data2(depth) [.data1.4.2.3.list get [.data1.4.2.3.list curselection]]
    }
  }

}

##############################################################################

proc DataSaveConfigFile {} {

  global Data2 Fs Dir fname NameData

set NameData(1) {DBDescrname}
set NameData(2) {OutGenName}
set NameData(3) {yearmin}
set NameData(4) {yearmax}
set NameData(5) {permin}
set NameData(6) {permax}
set NameData(7) {ContDescr}
set NameData(8) {ContDescrName}
set NameData(9) {depthlist} 
set NameData(10) {latmin}
set NameData(11) {latmax}
set NameData(12) {lonmin}
set NameData(13) {lonmax}
set NameData(14) {ctd}
set NameData(15) {bottle}
set NameData(16) {bt}
set NameData(17) {TS} 
set NameData(18) {GlobalFlagMin}
set NameData(19) {GlobalFlagMax}
set NameData(20) {FlagMin}
set NameData(21) {FlagMax}

 set fname [tk_getSaveFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -title {Save as...}
]


set FileID [open $fname w+]

for {set x 1} {$x<22} {incr x} {
    puts $FileID $Data2($NameData($x)) 
}

close $FileID
return

}

##############################################################################

proc DataLoadConfigFile {} {

  global Data2 Fs Dir DataConfFile NameData

set NameData(1) {DBDescrname}
set NameData(2) {OutGenName}
set NameData(3) {yearmin}
set NameData(4) {yearmax}
set NameData(5) {permin}
set NameData(6) {permax}
set NameData(7) {ContDescr}
set NameData(8) {ContDescrName}
set NameData(9) {depthlist} 
set NameData(10) {latmin}
set NameData(11) {latmax}
set NameData(12) {lonmin}
set NameData(13) {lonmax}
set NameData(14) {ctd}
set NameData(15) {bottle}
set NameData(16) {bt}
set NameData(17) {TS} 
set NameData(18) {GlobalFlagMin}
set NameData(19) {GlobalFlagMax}
set NameData(20) {FlagMin}
set NameData(21) {FlagMax}

set DataConfFile [tk_getOpenFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -initialfile adriatic_data.conf\
  -title {Select a file...}
]
  
set filename [open $DataConfFile]
set lineNumber 0
set i 1
 while {[gets $filename line] >= 0} {
 set Data2($NameData($i)) $line
 incr i 1
}

}

################################################################################

proc Data_Extract {} {

  global Data Data2 Dir Dict DescrFile 
# ----------------------------------------------------------------------------
# Test for DB description file 
# ----------------------------------------------------------------------------

  set fDBlistid [open $Data(DBDescrname) r]
  gets $fDBlistid line
  close $fDBlistid

  set ext [CutString last $line 3]
  if { $ext == "pdf" } {
    set DBformat 1
  } else {
    set ext [CutString last $line 2]
    if { $ext == "nc" } {
      set DBformat 2
    } else {
      set ext [CutString last $line 3]
      if { $ext == "med" } {
        set DBformat 3
      } else {
        ErrorMessageBox Error $Dict(BadFileExtension)
        return
      }
    }
  } 

# ----------------------------------------------------------------------------
# Test for periods 
# ----------------------------------------------------------------------------

  set testper $Data(yearmin)$Data(yearmax)$Data(permin)$Data(permax)
  set testper2 [regexp {^[0-9]+$} $testper]

  if {!$testper2} {
    ErrorMessageBox Error $Dict(PeriodsMustBeIntegerValue)
    return
  }
 
# if  

# ----------------------------------------------------------------------------
# Test for latitude and longitude
# ----------------------------------------------------------------------------

  if {!$Data(ContDescr)} {
    if [catch {
        set  latmin [format "%8.4f" $Data(latmin)] 
        set  latmax [format "%8.4f" $Data(latmax)] 
        set  lonmin [format "%8.4f" $Data(lonmin)] 
        set  lonmax [format "%8.4f" $Data(lonmax)]
    }]  { 
        set errtxt $Dict(BadPositionValue)
    } else {
      if { ($latmin < $latmax) && ($lonmin < $lonmax) } {
        if { ([expr abs($latmin)] <= 90.) && ([expr abs($latmax)] <= 90.) && \
             ([expr abs($lonmin)] <= 180.) && ([expr abs($lonmax)] <= 180.) } {
          set errtxt 0 
        } else {
          set errtxt $Dict(VerifyLatitude1)
        }
      } else {
        set errtxt $$Dict(VerifyLatitude2)
      }
    }

    if { $errtxt != 0 } {
      ErrorMessageBox Error $errtxt 
      return
    }
  }

# ----------------------------------------------------------------------------
# Test for flags
# ----------------------------------------------------------------------------

  set fl $Data(GlobalFlagMin)$Data(GlobalFlagMax)$Data(FlagMin)$Data(FlagMax)
  set flag [regexp {^[0-9]+$} $fl]
  if {!$flag} {
    ErrorMessageBox Error $Dict(ChooseFlags)
    return
  }

# ----------------------------------------------------------------------------
# Test for CTD, BOTTLES, BT
# ----------------------------------------------------------------------------

  if {$Data(ctd)} {set ctd "Y"} else {set ctd "N"}
  if {$Data(bottle)} {set bottle "Y"} else {set bottle "N"}
  if {$Data(bt)} {set bt "Y"} else {set bt "N"}
  
# ----------------------------------------------------------------------------
# Remove old files
# ----------------------------------------------------------------------------

  set data_old_files [glob -nocomplain $Data(OutGenName).*]
  if {$data_old_files != {}} {
    set but [YesNoMessageBox $Dict(Empty) $Dict(EraseOldDataFiles) $Dict(No) ] 
    if {$but == "Yes"} {
      foreach old $data_old_files {
        catch {exec rm $old 2> /dev/null}
      }
    } 
  }
  puts "extract"
# ----------------------------------------------------------------------------
# Extract !
# ----------------------------------------------------------------------------

  set nbprof [llength $Data(depthlist)]

  set depthfile $Dir(depth)/[RelName $Data(OutGenName)].depth
  puts $depthfile
  set fdepthid [open $depthfile w]

# set fextrid stdout 

# ----------------------
# execution de extract.a
# ----------------------

   set fextrid [open |$Data(extr_exec) w]

 # set dir [pwd]
 # set fextrid [open $dir/test.output w]


  puts $fextrid $DBformat
  puts $fextrid $Data(DBDescrname)
  puts $fextrid "5"
  puts $fextrid $Data(OutGenName)
# puts $fextrid [format "%4d,%4d" $Data(yearmin) $Data(yearmax)]
# puts $fextrid [format "%4d,%4d" $Data(permin) $Data(permax)]
  puts $fextrid $Data(yearmin),$Data(yearmax)
  puts $fextrid $Data(permin),$Data(permax)
  puts $fextrid "0"
  if {$Data(ContDescr)} {
    puts $fextrid $Data(ContDescrName)
    puts $fextrid "0.,0."
    puts $fextrid "0.,0."
  } else {
    puts $fextrid "*"
    puts $fextrid "$latmin,$latmax"
    puts $fextrid "$lonmin,$lonmax"
  }
  puts $fextrid $ctd$bottle$bt   
  puts $fextrid $Data(GlobalFlagMin),$Data(GlobalFlagMax)
  puts $fextrid $Data(TS)
  puts $fextrid $Data(FlagMin),$Data(FlagMax)
  puts $fextrid $nbprof
  foreach depth $Data(depthlist) {
    puts $fextrid $depth.
  } 

  set i 0
  foreach depth $Data(depthlist) {
    set i [expr $i+1]
    puts $fdepthid [format "%5.0f        $Data2(OutGenName).%02d" $depth $i]
    puts [format "%5.0f        $Data2(OutGenName).%02d" $depth $i]
  }

  flush $fextrid
  close $fdepthid
  if [catch {close $fextrid} errtmp] {
    ErrorMessageBox $Dict(ErrorInExtractionProcedure) $errtmp 
  }
} 

# -------------------------------------------------------------------
proc VisuDataOutputFile {} {
global Fs Dir Dict filename Data2 w 

set filename [
tk_getOpenFile -defaultextension .data\
  -initialdir $Dir(data) \
  -initialfile $Data2(OutGenName)\
  -title $Dict(OpenDataFile)
]

#set filename $Data2(OutGenName)


set fileid [open $filename] 
set full_text [read $fileid]

set w .text
catch {destroy $w}
toplevel $w
wm title $w $Dict(OpenDataFile)
wm iconname $w "text"

label $w.label -text $filename 
pack $w.label -side top -fill x
text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set"\
-setgrid 1 -height 30
scrollbar $w.scroll -command "$w.text yview"

pack $w.scroll -side right -fill y
pack $w.text -expand yes -fill both

$w.text insert 0.0 $full_text 
button $w.button -text $Dict(Close) -command {destroy $w} -width 30 -relief raised
pack $w.button -expand 1 -fill x
}




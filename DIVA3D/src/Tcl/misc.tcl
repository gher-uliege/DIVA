#######################################################
# Set array b to array a
#######################################################

proc CopArr { a b } {
  upvar $a arr1
  upvar $b arr2
  foreach i [array names arr1] {
     set arr2($i) $arr1($i)
  }
}

#######################################################
#Return $dn with "/" if missing
#######################################################

proc DirSlash { dn } {
  if {[expr [string length $dn] -1] != [string last "/" $dn]} {
    append dn "/"
  }
  return $dn
}

#######################################################
# Return remaining string after the last "/"
#######################################################

proc RelName { dn } {
  set rl [string range $dn [expr [string last "/" $dn] +1] [string length $dn]]
  return $rl
}
 
#######################################################
# Return string before the last "/"
#######################################################

proc PathName { dn } {
  if {[string last "/" $dn] != [expr [string length $dn] -1]} { 
    set pn [string range $dn 0 [expr [string last "/" $dn] -1]]
  } else {
    set pn [string range $dn 0 [expr [string length $dn] -2]]
  }
  return $pn
}
 

#######################################################
# p_quit Procedure
#######################################################

proc p_quit {} {

 global Dict
 if { [YesNoMessageBox $Dict(QuitDiva) $Dict(ReallyQuit) $Dict(No) ] \
                       == $Dict(Yes) } {
  SaveVar
 }
}

#######################################################
# Save variables when leaving
#######################################################

proc SaveVar {} {

  global env Data Diva Fem GrData GrFem GrDiva Dir Coordinates Preference Panel

  set Panel(data) [DivaGetWindowGeometry .data1]
  set Panel(mesh) [DivaGetWindowGeometry .fem1]
  set Panel(diva) [DivaGetWindowGeometry .diva]
  set Panel(grdata) [DivaGetWindowGeometry .view1]
  set Panel(grmesh) [DivaGetWindowGeometry .view2]
  set Panel(grdiva) [DivaGetWindowGeometry .view3]
  set Panel(coord) [DivaGetWindowGeometry .options1]
  set Panel(dir) [DivaGetWindowGeometry .options2]
  set Panel(language) [DivaGetWindowGeometry .options3]
 
  set fvar [open $env(HOME)/.diva w]
  foreach i {Data Diva Fem GrData GrFem GrDiva Dir \
		Coordinates Preference Panel} {
    puts $fvar "VALUES FOR THE $i ARRAY"
    upvar 0 $i\(VarList\) vlist
    foreach arg $vlist {
      if {[info exists $i\($arg\)]} {
        upvar 0 $i\($arg\) var
        puts $fvar $var
      } else {
        puts $fvar ""
      }
    }
  }
  exit
}

#######################################################
# Load variables when starting
#######################################################

proc LoadVar {} {

  global env Data Diva Fem GrData GrFem GrDiva  \
		Dir Coordinates Preference Fs Panel Dict

  if {[file exists $env(HOME)/.diva]} {
    set fvar [open $env(HOME)/.diva r]
    foreach i {Data Diva Fem GrData GrFem GrDiva \
		Dir Coordinates Preference Panel} {
      gets $fvar tmp
      upvar $i\(VarList\) vlist
      foreach arg $vlist {
        upvar $i\($arg\) var
        gets $fvar var
      }
    }
  } else {
    set Data(DBDescrname) $Fs(data_base_des)
   # set Data(OutGenName) $Fs(data_name)/test.data
    set Data(OutGenName) $Fs(data_name)
    set Data(yearmin) 1960
    set Data(yearmax) 1990
    set Data(permin) 0101
    set Data(permax) 1231
    set Data(ContDescr) 1
    set Data(ContDescrName) $Fs(data_cont)
    set Data(depthlist) {}
    set Data(latmin) 30
    set Data(latmax) 40
    set Data(lonmin) 2
    set Data(lonmax) 10
    set Data(ctd) 1
    set Data(bottle) 1
    set Data(bt) 1
    set Data(TS) 1
    set Data(GlobalFlagMin) 0
    set Data(GlobalFlagMax) 6
    set Data(FlagMin) 0
    set Data(FlagMax) 6

    set Diva(femdim) 2
    set Diva(DataName) $Fs(diva_data)
    set Diva(DepthName) $Fs(diva_data_depth)
    set Diva(MeshName) $Fs(diva_mesh)
    set Diva(BathName) $Fs(diva_bath)
    # set Diva(OutName) $Fs(diva_name)/test.anl
    set Diva(OutName) $Fs(diva_name)
    set Diva(error) 0
    set Diva(Varbak) 1.0
    set Diva(dt) 0
    set Diva(RBTcl) 1
    set Diva(clref) 10.0
    set Diva(muref) 0.000001
    set Diva(cl) 1
    set Diva(snr) 1
    set Diva(alph0) 1
    set Diva(alph1) 2
    set Diva(mu) 12.5664
    set Diva(xorig) ""
    set Diva(yorig) ""
    set Diva(dx) .25 
    set Diva(dy) .25 
    set Diva(xend) ""
    set Diva(yend) ""
    set Diva(valexcl) "-99999"
    set Diva(SelDepth) 0
    set Diva(ov) 1

    set Fem(dim) 2
    set Fem(contname) $Fs(mesh_cont)
    # set Fem(meshname) $Fs(mesh_name)/test.mesh
    set Fem(meshname) $Fs(mesh_name)
    set Fem(batname) $Fs(mesh_bat)
    set Fem(depthname) $Fs(mesh_depth)
    set Fem(unif) 0
    set Fem(rlen) 0.5
    set Fem(srcoef) 1.5
    set Fem(nbsm) 5

    set GrData(dim) 2
    set GrData(name) $Fs(gr_data_name) 
    set GrData(DepthName) $Fs(gr_data_name_depth)
    set GrData(ContDescr) 0
    set GrData(ContDescrName) ""
    set GrData(seldepth) 0
    set GrData(title) "Data visualisation"
    set GrData(xname) Longitude
    set GrData(yname) Latitude

    set GrFem(femdim) 2
    set GrFem(name) $Fs(gr_mesh_name) 
    set GrFem(depthname) $Fs(gr_mesh_depth) 
    set GrFem(seldepth) 0
    set GrFem(title) "Mesh visualisation"
    set GrFem(xname) Longitude
    set GrFem(yname) Latitude

    set GrDiva(resname) $Fs(gr_diva_name)
    set GrDiva(seldepth) 0
    set GrDiva(dispdepth) 0
    set GrDiva(title) "Analyse visualisation"
    set GrDiva(xname) Longitude
    set GrDiva(yname) Latitude
    set GrDiva(clrlv) 30
    set GrDiva(depthscale) 0.

    set Dir(mesh) $env(DIVA_WORK_DIR)/MESH
    set Dir(data) $env(DIVA_WORK_DIR)/DATA
    set Dir(analysis) $env(DIVA_WORK_DIR)/ANALYSIS
    set Dir(temp) $env(DIVA_WORK_DIR)/TMP
    set Dir(database) $env(DIVA_WORK_DIR)/BASE
    set Dir(options) $env(DIVA_WORK_DIR)/CONFIG
    set Dir(description) $env(DIVA_WORK_DIR)/DES
    set Dir(bathymetry) $env(DIVA_WORK_DIR)/BAT
    set Dir(contour) $env(DIVA_WORK_DIR)/CONT
    set Dir(depth) $env(DIVA_WORK_DIR)/DEPTH
    set Dir(work) $env(DIVA_WORK_DIR)

    set Coordinates(system) 0
    set Coordinates(lambda) 40

    set Preference(language) English

    set Panel(data) ""
    set Panel(mesh) ""
    set Panel(diva) ""
    set Panel(grdata) ""
    set Panel(grmesh) ""
    set Panel(grdiva) ""
    set Panel(coord) ""
    set Panel(dir) ""
    set Panel(language) ""
  }
}

#######################################################
proc DivaRestoreEnvironment { } {

      global Panel Dict

      if { $Panel(data) !="" } {
       p_data1
       wm geometry .data1 $Panel(data)
      }
      if { $Panel(mesh) !=""} {
       p_fem1
       wm geometry .fem1 $Panel(mesh)
      }
      if { $Panel(diva) !=""} {
       p_diva
       wm geometry .diva $Panel(diva)
      }
      if { $Panel(grdata) !=""} {
       p_view1
       wm geometry .view1 $Panel(grdata)
      }
      if { $Panel(grmesh) !=""} {
       p_view2
       wm geometry .view2 $Panel(grmesh)
      }
      if { $Panel(grdiva) !=""} {
       p_view3
       wm geometry .view3 $Panel(grdiva)
      }
      if { $Panel(coord) !=""} {
       p_options1
       wm geometry .options1 $Panel(coord)
      }
      if { $Panel(dir) !=""} {
       p_options2
       wm geometry .options2 $Panel(dir)
      }
      if { $Panel(language) !=""} {
       p_options3
       wm geometry .options3 $Panel(language)
      }

}

#######################################################
# Return string st without the n last character (fl=first)
# Return the n last character of string st (fl=last)
#######################################################

proc CutString { fl st { n  4 }} {

  set len [string length $st]

  if { $fl == "first" } {
    set name [string range $st 0 [expr $len-$n-1]]
    return $name
  } 
  
  if { $fl == "last" } {
    set name [string range $st [expr $len-$n] [expr $len-1]] 
    return $name
  } 

} 
#######################################################
# Load Configuration File
#######################################################

proc LoadConfigFile { ar vl di { filter *} } {

  global $di Dict
  upvar $ar arr
  upvar $vl vlist
  upvar $di dival
  set fname 0

  #DivaFileSelect .fs -full 0 -title $Dict(LoadConf) \
  #  -dir $dival -filter $filter


  tk_getOpenFile -initialdir $dival\
  -defaultextension .conf\
  -title $Dict(LoadConf)\
  -initialdir $dival\
  -initialfile {default.data.conf}
  
  

return
} 

#######################################################
# Save Configuration File
#######################################################

proc SaveConfigFile { ar vl di { filter * } } {

  global $di Dict
  upvar $ar arr
  upvar $vl vlist
  upvar $di dival
  set fname 0


   tk_getSaveFile \
	-defaultextension .conf\
	-initialdir $dival\
	-initialfile {default.data.conf}\
	-title $Dict(SaveConf) 

return
}

#######################################################
# Load depth list
#######################################################

proc LoadDepthList {nm dp {dt 0} {fline 0}} {

  upvar $nm name
  upvar $dp depthlist
  if {$dt != 0} {upvar $dt datanamelist}

  set depthlist {}
  set datanamelist {}

  set fileid [open $name r]
  for {set i 0} {$i < $fline} {incr i 1} {
    get $fileid line
  }
  while { [gets $fileid line] > 0 } {
    lappend depthlist [string range $line 0 4]
    if {$dt != 0} {lappend datanamelist [string range $line 13 end]}
  }
  close $fileid
}


#######################################################
# Save Dir informations
#######################################################

proc WriteDirFile {} {

  global env Dir

  set fdirid [open $env(HOME)/.diva.dir w]
  puts $fdirid $Dir(Mesh)
  puts $fdirid $Dir(Data)
  puts $fdirid $Dir(Results)
  puts $fdirid $Dir(Temp)
  puts $fdirid $Dir(DataBase)
  puts $fdirid $Dir(Options)
  close $fdirid

}

#######################################################
# Set two variables to be the same : set a(aa) $b(bb)
#######################################################

proc VarEqu { a aa b bb {ext ""}} {

  global $a $b

  if {[info exists $b\($bb\)]} {
    upvar $b\($bb\) second
    if {![Empty $second]} {
      if {[info exists $a\($aa\)]} {
        upvar $a\($aa\) first
        if {[Empty $first]} {uplevel set $a\($aa\) $second$ext}
      } else {
        uplevel set $a\($aa\) $second$ext
      }
    }
  } else {
    set $b\($bb\) ""
  }

}
   
#######################################################
# 0 if arg is empty or 1 if not
#######################################################

proc Empty { arg } {

  set a [split $arg " "]
  set b [join $a ""]
  if {$b == ""} {
    return 1
  } else {
    return 0
  }

}

#######################################################
# Return the first element of a string with space
#######################################################

proc FirstElemStr { str } {

  set list [split $str " "]
  set i 0
  set first ""
  while {$first == ""} {
    set first [lindex $list $i]
    incr i
  }
  return $first
}

#######################################################


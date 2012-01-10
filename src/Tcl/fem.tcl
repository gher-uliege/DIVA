#######################################################
# p_fem1 Procedure
#######################################################

proc p_fem1 {} {

   global Fem Fem2 Data Dict

  if { [winfo exist .fem1] } { wm deiconify .fem1
				raise .fem1
                               return }

   CopArr Fem Fem2

   VarEqu Fem contname Data ContDescrName

   VarEqu Fem depthname Data OutGenName .depth

   toplevel .fem1 -bd 5 -relief ridge
   wm title .fem1 $Dict(MeshCreation) 
   wm geometry .fem1 +400+75
   MakeFrame .fem1 5

#--------------------------------------------------------------

#   label .fem1.1.label -text $Dict(MeshCreation) 
#   .fem1.1.label config -width 50
#   pack .fem1.1.label -side top

#--------------------------------------------------------------

   radiobutton .fem1.2.radio1 -text $Dict(2D) -relief flat \
        -variable Fem2(dim) -value 2 -command {FemSwitch2D3D}
   radiobutton .fem1.2.radio2 -text $Dict(3D) -relief flat \
        -variable Fem2(dim) -value 3 -command {FemSwitch2D3D}

   pack .fem1.2.radio1 -side left
   pack .fem1.2.radio2 -side right

#--------------------------------------------------------------

   MakeFrame .fem1.3 12 flat

   label .fem1.3.1.label -text $Dict(ContourDescriptionFile) -anchor w
   button .fem1.3.1.load -text $Dict(Load) -padx 5 -command {Fem1LoadCont}
   entry .fem1.3.2.entry -relief sunken -width 55 -textvariable Fem2(contname)

   pack .fem1.3.1.label -side left
   pack .fem1.3.1.load -side right -padx 10
   pack .fem1.3.2.entry -side left -fill x -expand 1

   label .fem1.3.3.label -text $Dict(GenericMeshOuputFileName)
   button .fem1.3.3.load -text $Dict(Load) -padx 5 -command {Fem1SelectMesh}
   entry .fem1.3.4.entry -relief sunken -textvariable Fem2(meshname)

   pack .fem1.3.3.label -side left
   pack .fem1.3.3.load -side right -padx 10
   pack .fem1.3.4.entry -side left -fill x -expand 1

   button .fem1.3.5.label -text $Dict(BathymetryFileName) -relief flat
   button .fem1.3.5.load -text $Dict(Load) -padx 5 -command {Fem1LoadBat}
   entry .fem1.3.6.entry -relief sunken -textvariable Fem2(batname)
   bind .fem1.3.6.entry <FocusIn> {focus .fem1}
 
   pack .fem1.3.5.label -side left
   pack .fem1.3.5.load -side right -padx 10
   pack .fem1.3.6.entry -side left -fill x -expand 1

   button .fem1.3.7.label -text $Dict(DepthFileName) -relief flat
   button .fem1.3.7.load -text $Dict(Load) -padx 5 -command {Fem1LoadDepth}
   entry .fem1.3.8.entry -relief sunken -textvariable Fem2(depthname)
#   bind .fem1.3.8.entry <FocusIn> {focus .fem1}

   pack .fem1.3.7.label -side left
   pack .fem1.3.7.load -side right -padx 10
   pack .fem1.3.8.entry -side left -fill x -expand 1

   radiobutton .fem1.3.9.radio3 -text $Dict(UniformMesh) \
     -relief flat -anchor w -variable Fem2(unif) -value 0
   radiobutton .fem1.3.9.radio4 -text $Dict(NonUniformMesh) \
     -relief flat -anchor w -variable Fem2(unif) -value 1
   label .fem1.3.10.label -text $Dict(ReferenceLength) 
   entry .fem1.3.10.entry -relief sunken -textvariable Fem2(rlen) -width 15
   label .fem1.3.11.label -text $Dict(SurfaceCoef)
   entry .fem1.3.11.entry -relief sunken -textvariable Fem2(srcoef) -width 15
   label .fem1.3.12.label -text $Dict(SmoothNumber) 
   entry .fem1.3.12.entry -relief sunken -textvariable Fem2(nbsm) -width 15

   pack .fem1.3.9.radio3 -side top -anchor w
   pack .fem1.3.9.radio4 -side top -anchor w
   pack .fem1.3.10.label -side left
   pack .fem1.3.10.entry -side right
   pack .fem1.3.11.label -side left
   pack .fem1.3.11.entry -side right
   pack .fem1.3.12.label -side left
   pack .fem1.3.12.entry -side right

#--------------------------------------------------------------

   MakeFrame .fem1.4 2 flat

   button .fem1.4.1.loaddescr -text $Dict(LoadConf) -width 16 -command \
     {FemLoadConfigFile}

   button .fem1.4.1.savedescr -text $Dict(SaveConf) -width 16 -command \
     {FemSaveConfigFile}

   button .fem1.4.2.ok -text OK -width 16 -command {
     CopArr Fem2 Fem
     CreateMesh
     CopyMeshFile
   } 
   button .fem1.4.2.cancel -text $Dict(Cancel) -width 16 -command {
     QuitMenu .fem 1
   }
   pack .fem1.4.1.loaddescr .fem1.4.1.savedescr -side left -fill x -expand 1 \
     -padx 5 -pady 5 
   pack .fem1.4.2.ok .fem1.4.2.cancel -side left -fill x -expand 1 \
     -padx 5 -pady 5 

   FemSwitch2D3D


#--------------------------------------------------------------

   MakeFrame .fem1.5 1 flat

   
   button .fem1.5.1.cont -text $Dict(VisuCont) -relief raised\
   -width 16 -command {VisuContourFile }

   button .fem1.5.1.mesh -text $Dict(VisuMesh) -relief raised\
   -width 16 -command {VisuMeshOutputFile}
  

   pack .fem1.5.1.cont -side left -padx 5 -pady 5 -fill x -expand 1
   pack .fem1.5.1.mesh -side left -padx 5 -pady 5 -fill x -expand 1
				 
}


##############################################################################

proc FemSaveConfigFile {} {

  global Fem2 Fs Dir FemConfFile NameFem
  

  set NameFem(1) {dim}
  set NameFem(2) {contname}
  set NameFem(3) {meshname}
  set NameFem(4) {batname}
  set NameFem(5) {depthname}
  set NameFem(6) {unif}
  set NameFem(7) {rlen}
  set NameFem(8) {srcoef}
  set NameFem(9) {nbsm} 


  set fname [tk_getSaveFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -title {Save as...}
]

set FileID [open $fname w+]

for {set x 1} {$x<10} {incr x} {
    puts $FileID $Fem2($NameFem($x)) 
}

close $FileID
return


}

##############################################################################

proc FemLoadConfigFile {} {

global Fem2 Fs Dir FemConfFile NameFem

set NameFem(1) {dim}
set NameFem(2) {contname}
set NameFem(3) {meshname}
set NameFem(4) {batname}
set NameFem(5) {depthname}
set NameFem(6) {unif}
set NameFem(7) {rlen}
set NameFem(8) {srcoef}
set NameFem(9) {nbsm} 

set FemConfFile [tk_getOpenFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -initialfile adriatic_mesh.conf\
  -title {Select a file...}
]
  
set filename [open $FemConfFile]
set lineNumber 0
set i 1
 while {[gets $filename line] >= 0} {
 set Fem2($NameFem($i)) $line
 incr i 1
}


  FemSwitch2D3D
}

##############################################################################

proc FemSwitch2D3D {} {

 global Fem2 active_foreground disable_foreground

 if {$Fem2(dim) == 2} {
  .fem1.3.5.label configure -state disabled
  .fem1.3.7.label configure -state disabled
  .fem1.3.5.load  configure -state disabled
  .fem1.3.7.load configure -state disabled
  .fem1.3.6.entry configure -foreground $disable_foreground 
  .fem1.3.8.entry configure -foreground $disable_foreground
   bind .fem1.3.6.entry <FocusIn> {focus .fem1}
 } else {
#  .fem1.3.6.entry -state normal
  .fem1.3.5.label configure -state normal
  .fem1.3.7.label configure -state normal
  .fem1.3.5.load configure -state normal
  .fem1.3.7.load configure -state normal
  .fem1.3.6.entry configure -foreground $active_foreground 
  .fem1.3.8.entry configure -foreground $active_foreground
   bind .fem1.3.6.entry <FocusIn> {focus .fem1.3.6.entry}
 }

}

#######################################################
# Fem1LoadCont procedure
#######################################################

#proc Fem1LoadCont {} {
#
#  global Fem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(ContourDescriptionFile) \
#	-filter $Fs(mesh_cont_filter)  -full 0 -dir $Dir(contour)
#
#  if {[.fs activate]} {
#    set Fem2(contname) [.fs get] 
#  }
#  .fs delete
#  return
#
#}

proc Fem1LoadCont {} {
global Fem2 Fs Dir Dict

set Fem2(contname) [
 tk_getOpenFile  -defaultextension .mesh.conf\
 -initialdir $Dir(contour) \
 -initialfile adriatic.cont\
 -title $Dict(ContourDescriptionFile)
]
return
}



#######################################################
# Fem1SelectMesh procedure
#######################################################

#proc Fem1SelectMesh {} {
#
#  global Fem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(MeshFileName) \
#    -filter $Fs(mesh_name_filter) -full 0 -dir $Dir(mesh)
#
#  if {[.fs activate]} {
#    set fname [.fs get]
#    set Fem2(meshname) $fname
#  }
#  .fs delete
#  return
#
#}

proc Fem1SelectMesh {} {

global Fem2 Fs Dir Dict

set Fem2(meshname) [
 tk_getSaveFile  -defaultextension .mesh\
 -initialdir $Dir(mesh) \
 -title $Dict(MeshFileName)\
 -initialfile adriatic.mesh\
 -initialfile {adriatic.mesh} 
]
return
}


#######################################################
# Fem1LoadBat procedure
#######################################################

#proc Fem1LoadBat {} {
#
#  global Fem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(BathymetryFileName) -full 0  \
#	-dir $Dir(bathymetry) -filter $Fs(mesh_bat_filter)
#
#  if {[.fs activate]} {
#    set Fem2(batname) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Fem1LoadBat {} {
global Fem2 Fs Dir Dict

set Fem2(batname) [
 tk_getOpenFile  -defaultextension .bat\
 -initialdir $Dir(bathymetry) \
 -title $Dict(BathymetryFileName)\
 -initialfile {adriatic2.bat}\
]
return
}

#######################################################
# Fem1LoadDepth procedure
#######################################################

#proc Fem1LoadDepth {} {
#
#  global Fem2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DepthFileName) -full 0 \
#    -dir $Dir(depth)  -filter $Fs(mesh_depth_filter)
#
#  if {[.fs activate]} {
#    set Fem2(depthname) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Fem1LoadDepth {} {
global Fem2 Fs Dir Dict

set Fem2(depthname) [
 tk_getOpenFile  -defaultextension .depth\
 -initialdir $Dir(depth) \
 -initialfile {adriatic.data.depth}\
 -title $Dict(DepthFileName) 
]
return
}

#######################################################
# CreateMesh procedure
#######################################################

proc CreateMesh {} {

  global Fem Dir Dict Coordinates

  if { $Fem(dim) == 3 } {
   if {![info exists Fem(batname)] || ![file exists $Fem(batname)]} {
    ErrorMessageBox Error $Dict(BadBathymetryFile)
    return 
   }
   if {![file exists $Fem(batname).info ]} {
    ErrorMessageBox Error $Dict(NoInfoFileAssociatedToBathymetry)
    return
   }
  }
  if { $Fem(unif) == 1 } {
   if {![file exists $Fem(contname).dens ]} {
    ErrorMessageBox Error $Dict(NoDensityFileAssociatedWithContour)
    return
   }
  }

  if { $Coordinates(system)==0} {
   set file_15 $Fem(contname)
  } else {
    set fcoord [open |$Fem(coord_exec) a]
    puts $fcoord "contour"
    puts $fcoord "ANGTOXY"
    puts $fcoord $Fem(contname)
    puts $fcoord $Fem(contname).xy
    puts $fcoord $Coordinates(lambda)
    if [catch {close $fcoord} err] {
     ErrorMessageBox $Dict(ErrorInCoord) $err
    return
    }
#    close $fcoord
    set  file_15 $Fem(contname).xy
  }

  set file_16 $Fem(meshname).mh2
  set file_17 $Fem(contname).dens

  set f16id [open $file_16 w]
  puts $f16id $Fem(unif)
  puts $f16id $Fem(rlen)
  puts $f16id $Fem(srcoef)
  puts $f16id $Fem(nbsm)
  close $f16id

# copie vers les unites fortran

  exec cp $file_15 $Dir(temp)/fort.10
  exec cp $file_16 $Dir(temp)/fort.11
  if { $Fem(unif) == 1 } {
    exec cp $file_17 $Dir(temp)/fort.12
  }

  set curdir [pwd]
  cd $Dir(temp)

###########################################
# modified November 30, 2006
#
# generopt instead of gener + reorg
###########################################

#  set fgener [open |$Fem(gener_exec) w]
#  if [catch {close $fgener} err] {
#    cd $curdir
#    FemPost 0 
#    ErrorMessageBox $Dict(ErrorInGener) $err 
#    return
#  }
#
#  set freorg [open |$Fem(reorg_exec) w]
#  if [catch {close $fgener} err] {
#    cd $curdir
#    FemPost 0
#    ErrorMessageBox $Dict(ErrorInReorg) $err 
#    return
#  }

  set fgeneropt [open |$Fem(generopt_exec) w]
  if [catch {close $fgeneropt} err] {
    cd $curdir
    FemPost 0
    ErrorMessageBox $Dict(ErrorInGeneropt) $err 
    return
  }


# end of modifications
######################

  cd $curdir

  FemPost

  if { $Fem(dim) == 3 } { Fem_Create3DMesh } 

} 

#######################################################

proc FemPost { {flag 1} } {

  global Dir Fem Coordinates Dict
  
  foreach file {10 11 12 20 21} {
  #  catch {exec rm "$Dir(temp)/fort.$file" 2> /dev/null}
     catch {exec rm "$Dir(temp)/fort.$file" 2> $Trash}
  }
  if {$flag == 1} {
  # catch {exec mv "$Dir(temp)/fort.23" $Fem(meshname).mh4 2> /dev/null}
    catch {exec mv "$Dir(temp)/fort.23" $Fem(meshname).mh4 2> $Trash}

   if { $Coordinates(system)==0} {
    
 #catch {exec mv "$Dir(temp)/fort.22" $Fem(meshname) 2> /dev/null}
  catch {exec mv "$Dir(temp)/fort.22" $Fem(meshname) 2> $Trash}

   } else {
  #  catch {exec mv "$Dir(temp)/fort.22" $Fem(meshname).xy 2> /dev/null}
     catch {exec mv "$Dir(temp)/fort.22" $Fem(meshname).xy 2> $Trash}

 exec cp $Dir(temp)/fort.23 $Fem(meshname).mh4 
 exec cp $Dir(temp)/fort.22 $Fem(meshname)
 exec cp $Dir(temp)/fort.22 $Fem(meshname).xy  

    set fcoord [open |$Fem(coord_exec) a]
    puts $fcoord "mesh"
    puts $fcoord "XYTOANG"
    puts $fcoord $Fem(meshname).xy
    puts $fcoord $Fem(meshname)
    puts $fcoord $Coordinates(lambda)
    if [catch {close $fcoord} err] {
     ErrorMessageBox $Dict(ErrorInCoord) $err
     return
    }
   }
  }

}

#######################################################

proc Fem_Create3DMesh {} {

  global Fem Dict

  puts stdout $Fem(fem3d_exec)

  set ffem3did [open |$Fem(fem3d_exec) w]
# set ffem3did stdout 

  puts $ffem3did $Fem(batname)  
  puts $ffem3did $Fem(meshname).mh4
  puts $ffem3did $Fem(meshname)
  puts $ffem3did $Fem(batname).info  
  puts $ffem3did $Fem(depthname)  

  set fdepthid [open $Fem(depthname) r]
  set nbdepth 0
  while { [gets $fdepthid val] > 0 } {
    set nbdepth [expr $nbdepth + 1]
  }
  puts $ffem3did $nbdepth
  puts $ffem3did $Fem(meshname).mh5
# flush $ffem3did
  if [catch {close $ffem3did} err] {
    ErrorMessageBox $Dict(ErrorCreating3DMesh) $err 
  }
}


# -------------------------------------------------------------------
# Shows the ascii mesh file generated by Mesh Generation.
# nov 2006
# -------------------------------------------------------------------


proc VisuMeshOutputFile {} {
global Fs Dir Dict filename Fem2 w

#set filename [
#tk_getOpenFile -defaultextension .mesh\
#  -initialdir $Dir(mesh) \
#  -initialfile $Fem2(meshname)\
#  -title $Dict(OpenMeshFile)
#]


set filename $Fem2(meshname)

set fileid [open $filename] 
set full_text [read $fileid]

set w .text
catch {destroy $w}
toplevel $w
wm title $w $Dict(OpenMeshFile)
wm iconname $w "text"

label $w.label -text $filename 
pack $w.label -side top -fill x
text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set"\
-setgrid 1 -height 30 -width 50 
scrollbar $w.scroll -command "$w.text yview"

pack $w.scroll -side right -fill y
pack $w.text -expand yes -fill both

$w.text insert 0.0 $full_text 
button $w.button -text $Dict(Close) -command {destroy $w} -width 30 -relief raised
pack $w.button -expand 1 -fill x
}

# -------------------------------------------------------------------
# Shows the ascii contour file used by Mesh Generation.
# nov 2006
# -------------------------------------------------------------------

proc VisuContourFile {} {

global Fs Dir Dict filename Fem2 w

#set filename [
#tk_getOpenFile -defaultextension .cont\
#  -initialdir $Dir(contour) \
#  -initialfile $Fem2(contname)\
#  -title $Dict(OpenContourFile)
#
#]

set filename $Fem2(contname)
set fileid [open $filename] 
set full_text [read $fileid]

set w .text
catch {destroy $w}
toplevel $w
wm title $w $Dict(OpenContourFile)
wm iconname $w "text"

label $w.label -text $filename 
pack $w.label -side top -fill x
text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set"\
-setgrid 1 -height 30  -width 50
scrollbar $w.scroll -command "$w.text yview"

pack $w.scroll -side right -fill y
pack $w.text -expand yes -fill both

$w.text insert 0.0 $full_text 
button $w.button -text $Dict(Close) -command {destroy $w} -width 30 -relief raised
pack $w.button -expand 1 -fill x
}

# -------------------------------------------------------------------

proc CopyMeshFile {} {

global Dir Fem2 Coordinates Trash

 set Fem2(meshname) [AddMeshExtension $Fem2(meshname)]

 set NameMesh [CutString first $Fem2(meshname) 5]
 
 exec cp $Dir(temp)/fort.23 $NameMesh.mesh.mh4 
 if { $Coordinates(system)==0} {
 exec cp $Dir(temp)/fort.22 $NameMesh.mesh 
} else {
 exec cp $Dir(temp)/fort.22 $NameMesh.xy 
}


}

# -------------------------------------------------------------------


proc AddMeshExtension {meshfilename} {

set extension [CutString last $meshfilename 4]

if { $extension == "mesh" } {
   set meshfilename $meshfilename
  } else {
   set meshfilename $meshfilename.mesh
 }
}

#######################################################
# p_diva Procedure
#######################################################



proc p_diva {} {

  global Diva Diva2 Data Fem Dict

  VarEqu Diva femdim Fem dim

  VarEqu Diva DataName Data OutGenName .depth

  VarEqu Diva MeshName Fem meshname .tpo

  VarEqu Diva BathName Fem batname

  if { [winfo exist .diva] } { wm deiconify .diva 
 				raise .diva
                               return }

  CopArr Diva Diva2 

  toplevel .diva -bd 5 -relief ridge
  wm title .diva $Dict(DivaReconstruction) 
  wm minsize .diva 500 500
  wm geometry .diva =+700+0


# frame .diva.0 -relief raised -bd 2
# pack .diva.0 -side top -anchor nw -fill x
# label .diva.0.label -text $Dict(DivaReconstruction)
# .diva.0.label config -width 50
# pack .diva.0.label

#--------------------------------------------------------------

  MakeFrame .diva 9 

  radiobutton .diva.1.radio1 -text $Dict(2D) -padx 5 -variable Diva2(femdim) -value 2 -command {DivaSwitch2D3D} -relief flat
  radiobutton .diva.1.radio2 -text $Dict(3D) -padx 5 -variable Diva2(femdim) -value 3 -command {DivaSwitch2D3D} -relief flat
#  .diva.0.label config -width 50
  pack .diva.1.radio1 -side left 
  pack .diva.1.radio2 -side right 

#--------------------------------------------------------------

 # MakeFrame .diva.2 11 flat

 # button .diva.2.1.label -text $Dict(DataFileName) -relief flat
 # button .diva.2.1.load -text $Dict(Load) -padx 5 -command {Diva_LoadData}
 # entry .diva.2.2.entry -relief sunken -textvariable Diva2(DataName) 
#
#  pack .diva.2.1.label -side left
#  pack .diva.2.1.load -side right -padx 10 
#  pack .diva.2.2.entry -side left -fill x -expand 1

#  button .diva.2.3.label -text $Dict(DepthFileName) -relief flat
#  button .diva.2.3.load -text $Dict(Load) -padx 5 -command {Diva_LoadDepthData}
#  entry .diva.2.4.entry -relief sunken -textvariable Diva2(DepthName)
#
#  pack .diva.2.3.label -side left
#  pack .diva.2.3.load -side right -padx 10
#  pack .diva.2.4.entry -side left -fill x -expand 1
#
#  label .diva.2.5.label -text $Dict(MeshFileName) 
#  button .diva.2.5.load -text $Dict(Load) -padx 5 -command {Diva_LoadMesh}
#  entry .diva.2.6.entry -relief sunken -textvariable Diva2(MeshName) 
#
#  pack .diva.2.5.label -side left
#  pack .diva.2.5.load -side right -padx 10 
#  pack .diva.2.6.entry -side left -fill x -expand 1
#
#  button .diva.2.7.label -text $Dict(BathymetryFileName) -relief flat
#  button .diva.2.7.load -text $Dict(Load) -padx 5 -command {Diva_LoadBath}
#  entry .diva.2.8.entry -relief sunken -textvariable Diva2(BathName) 
#
#  pack .diva.2.7.label -side left
#  pack .diva.2.7.load -side right -padx 10 
#  pack .diva.2.8.entry -side left -fill x -expand 1
#
#  label .diva.2.9.label -text $Dict(AnalysisOutputFileName)
#  button .diva.2.9.load -text $Dict(Load) -padx 5 -command {Diva_LoadOut}
#  entry .diva.2.10.entry -relief sunken -textvariable Diva2(OutName) 
#  checkbutton .diva.2.11.check -relief flat -variable Diva2(error) \
#              -text $Dict(ComputeError) -onvalue 1 -offvalue 0 \
#              -command DivaSwitchError
#  button .diva.2.11.label -text $Dict(Varbak) -relief flat
#  entry .diva.2.11.entry -relief sunken -textvariable Diva2(Varbak) 
#
#  pack .diva.2.9.label -side left
#  pack .diva.2.9.load -side right -padx 10 
#  pack .diva.2.10.entry -side left -fill x -expand 1
#  pack .diva.2.11.check -side left 
#  pack .diva.2.11.entry .diva.2.11.label -side right 
#
#-------------

  MakeFrame .diva.2 6 flat

  button .diva.2.1.label -text $Dict(DataFileName) -relief flat
  entry .diva.2.1.entry -relief sunken -width 55 -textvariable Diva2(DataName) 
  button .diva.2.1.load -text $Dict(Load) -padx 5 -command {Diva_LoadData}
  pack .diva.2.1.label -side left
  pack .diva.2.1.load -side right -padx 5 
  pack .diva.2.1.entry -side right -padx 5

  #---------
  button .diva.2.2.label -text $Dict(DepthFileName) -relief flat
  entry .diva.2.2.entry -relief sunken -width 55 -textvariable Diva2(DepthName)
  button .diva.2.2.load -text $Dict(Load) -padx 5 -command {Diva_LoadDepthData}
  pack .diva.2.2.label -side left
  pack .diva.2.2.load -side right -padx 5
  pack .diva.2.2.entry -side right -padx 5

  #---------
  label .diva.2.3.label -text $Dict(MeshFileName) -relief flat
  entry .diva.2.3.entry -relief sunken -width 55 -textvariable Diva2(MeshName) 
  button .diva.2.3.load -text $Dict(Load) -padx 5 -command {Diva_LoadMesh}
  pack .diva.2.3.label -side left
  pack .diva.2.3.load -side right -padx 5 
  pack .diva.2.3.entry -side right -padx 5

  #---------
  button .diva.2.4.label -text $Dict(BathymetryFileName) -relief flat
  entry .diva.2.4.entry -relief sunken -width 55 -textvariable Diva2(BathName) 
  button .diva.2.4.load -text $Dict(Load) -padx 5 -command {Diva_LoadBath}
  pack .diva.2.4.label -side left
  pack .diva.2.4.load -side right -padx 5 
  pack .diva.2.4.entry -side right -padx 5

  #---------
  label .diva.2.5.label -text $Dict(AnalysisOutputFileName) -relief flat
  entry .diva.2.5.entry -relief sunken -width 55 -textvariable Diva2(OutName) 
  button .diva.2.5.load -text $Dict(Load) -padx 5 -command {Diva_LoadOut}
  pack .diva.2.5.label -side left
  pack .diva.2.5.load -side right -padx 5 
  pack .diva.2.5.entry -side right -padx 5

  #---------
  checkbutton .diva.2.6.check -relief flat -variable Diva2(error) \
              -text $Dict(ComputeError) -onvalue 1 -offvalue 0 \
              -command DivaSwitchError
  button .diva.2.6.label -text $Dict(Varbak) -relief flat
  entry .diva.2.6.entry -relief sunken -textvariable Diva2(Varbak) 
  pack .diva.2.6.check -side left 
  pack .diva.2.6.entry .diva.2.6.label -side right 


#--------------------------------------------------------------

  MakeFrame .diva.3 3 flat

  label .diva.3.1.label -text $Dict(ReferenceField)
  pack .diva.3.1.label

  radiobutton .diva.3.2.radio1 -relief flat -variable Diva2(dt) -value 0\
              -text $Dict(None) -command DivaSwitchAnalyseParameter
  radiobutton .diva.3.2.radio2 -relief flat -variable Diva2(dt) -value 1\
               -text $Dict(MeanValue) -command DivaSwitchAnalyseParameter
  radiobutton .diva.3.2.radio3 -relief flat -variable Diva2(dt) -value 2\
               -text $Dict(LinearRegression) -command DivaSwitchAnalyseParameter
  radiobutton .diva.3.2.radio4 -relief flat -variable Diva2(dt) -value 3\
               -text $Dict(SemiNormed) -command DivaSwitchAnalyseParameter

  pack .diva.3.2.radio1 .diva.3.2.radio2 .diva.3.2.radio3 .diva.3.2.radio4 -side left -expand 1

  button .diva.3.3.label1 -text $Dict(CaracteristicLength) -relief flat
  entry .diva.3.3.entry1 -relief sunken -textvariable Diva2(clref) 
  button .diva.3.3.label2 -text $Dict(Mu) -relief flat
  entry .diva.3.3.entry2 -relief sunken -textvariable Diva2(muref) 
  pack .diva.3.3.label1 .diva.3.3.entry1 .diva.3.3.label2 .diva.3.3.entry2 -side left -expand 1
#  pack .diva.3.3.entry -side left -fill x -expand 1



#--------------------------------------------------------------

# MakeFrame .diva.4 2 flat

#  frame .diva.4.1 -relief flat
#  pack .diva.4.1 -side left -anchor nw -fill y
#  frame .diva.4.2 -relief flat
#  pack .diva.4.2 -side top -anchor nw -fill x

#  radiobutton .diva.4.1.radio -variable Diva2(RBTcl) -value 1
#  pack .diva.4.1.radio -expand 1 -fill y

#  MakeFrame .diva.4.2 2 flat

#  button .diva.4.2.1.label -text $Dict(CaracteristicLength) -relief flat
#  entry .diva.4.2.1.entry -relief sunken -textvariable Diva2(cl) 
#  button .diva.4.2.2.label -text $Dict(SNRatio) -relief flat
#  entry .diva.4.2.2.entry -relief sunken -textvariable Diva2(snr)

#  pack .diva.4.2.1.entry .diva.4.2.1.label -side right
#  pack .diva.4.2.2.entry .diva.4.2.2.label -side right

#  bind .diva.4.2.1.entry <FocusOut> {SetDivaParam}
#  bind .diva.4.2.2.entry <FocusOut> {SetDivaParam}
##############################################################
# réarrangement pour moins de place verticalement 
##############################################################

  frame .diva.4.1 -relief flat
  pack .diva.4.1 -side left -anchor w -fill y -expand 1
  radiobutton .diva.4.1.radio  -variable Diva2(RBTcl) -value 1
  pack .diva.4.1.radio -expand 1 -side left

  frame .diva.4.2 -relief flat
  pack .diva.4.2 -side left -anchor n -fill y
  button .diva.4.2.label -text $Dict(CaracteristicLength) -relief flat
  entry .diva.4.2.entry -relief sunken -textvariable Diva2(cl)
  pack .diva.4.2.label .diva.4.2.entry  -side left

  frame .diva.4.3 -relief flat
  pack .diva.4.3 -side left -anchor e -fill y
  button .diva.4.3.label -text $Dict(SNRatio) -relief flat
  entry .diva.4.3.entry -relief sunken -textvariable Diva2(snr)
  pack .diva.4.3.label .diva.4.3.entry  -side left

  bind .diva.4.2.entry <FocusOut> {SetDivaParam}
  bind .diva.4.3.entry <FocusOut> {SetDivaParam}

#--------------------------------------------------------------
    
#  frame .diva.5.1 -relief flat
#  pack .diva.5.1 -side left -anchor nw -fill y
#  frame .diva.5.2 -relief flat
#  pack .diva.5.2 -side top -anchor nw -fill x
#
#  radiobutton .diva.5.1.radio -variable Diva2(RBTcl) -value 0
#  pack .diva.5.1.radio -expand 1 -fill y

#  MakeFrame .diva.5.2 3 flat

#  button .diva.5.2.1.label -text $Dict(Alpha0) -relief flat
#  entry .diva.5.2.1.entry -relief sunken -textvariable Diva2(alph0)

#  label .diva.5.2.2.label -text $Dict(Alpha1) 
#  entry .diva.5.2.2.entry -relief sunken -textvariable Diva2(alph1)

#  label .diva.5.2.3.label -text $Dict(Mu) 
#  entry .diva.5.2.3.entry -relief sunken -textvariable Diva2(mu)

#  pack .diva.5.2.1.entry .diva.5.2.1.label -side right
#  pack .diva.5.2.2.entry .diva.5.2.2.label -side right
#  pack .diva.5.2.3.entry .diva.5.2.3.label -side right

#########################################################
# réarrangement des widgets pour réduire étalement vertical
#########################################################

  frame .diva.5.1 -relief flat
  pack .diva.5.1 -side left -anchor w -fill y -expand 1
  radiobutton .diva.5.1.radio -variable Diva2(RBTcl) -value 0
  pack .diva.5.1.radio -expand 1 -fill y

  frame .diva.5.2 -relief flat
  pack .diva.5.2 -side left -anchor e -fill y
  label .diva.5.2.label -text $Dict(Alpha0) -relief flat
  entry .diva.5.2.entry -relief sunken -textvariable Diva2(alph0)

  frame .diva.5.3 -relief flat
  pack .diva.5.3 -side left -anchor e -fill y
  label .diva.5.3.label -text $Dict(Alpha1) 
  entry .diva.5.3.entry -relief sunken -textvariable Diva2(alpha1)

  frame .diva.5.4 -relief flat
  pack .diva.5.4 -side left -anchor e -fill y
  label .diva.5.4.label -text $Dict(Mu) 
  entry .diva.5.4.entry -relief sunken -textvariable Diva2(mu)

  pack .diva.5.2.entry .diva.5.2.label -side right
  pack .diva.5.3.entry .diva.5.3.label -side right
  pack .diva.5.4.entry .diva.5.4.label -side right

#--------------------------------------------------------------

  MakeFrame .diva.6 5 flat

  label .diva.6.1.title -text $Dict(OutputGrid) 
  pack .diva.6.1.title -pady 5

# ---------------
#  label .diva.6.2.label1 -text $Dict(Xorigin) -width 13
#  entry .diva.6.2.entry1 -relief sunken -width 10 -textvariable Diva2(xorig)
#  label .diva.6.2.label2 -text $Dict(Yorigin) -width 13
#  entry .diva.6.2.entry2 -relief sunken -width 10 -textvariable Diva2(yorig)
#
#  pack .diva.6.2.label1 .diva.6.2.entry1 .diva.6.2.label2 .diva.6.2.entry2 \
#    -side left
#
#  label .diva.6.3.label1 -text $Dict(dx) -width 13 
#  entry .diva.6.3.entry1 -relief sunken -width 10 -textvariable Diva2(dx)
#  label .diva.6.3.label2 -text $Dict(dy) -width 13
#  entry .diva.6.3.entry2 -relief sunken -width 10 -textvariable Diva2(dy)
#
#  pack .diva.6.3.label1 .diva.6.3.entry1 .diva.6.3.label2 .diva.6.3.entry2 \
#    -side left
#
#  label .diva.6.4.label1 -text $Dict(Xend) -width 13 
#  entry .diva.6.4.entry1 -relief sunken -width 10 -textvariable Diva2(xend)
#  label .diva.6.4.label2 -text $Dict(Yend) -width 13
#  entry .diva.6.4.entry2 -relief sunken -width 10 -textvariable Diva2(yend)
#
#  pack .diva.6.4.label1 .diva.6.4.entry1 .diva.6.4.label2 .diva.6.4.entry2 \
#    -side left
#
#  label .diva.6.5.label -text $Dict(ExclusionValue) -width 13
#  entry .diva.6.5.entry -relief sunken -width 10 -textvariable Diva2(valexcl)
#  checkbutton .diva.6.5.chk -variable Diva2(SelDepth) -width 18 \
#    -text $Dict(SelectedDepth) -onvalue 1 -offvalue 0
#
#  pack .diva.6.5.label .diva.6.5.entry .diva.6.5.chk -side left -pady 10 
#  pack .diva.6.5.chk -side right -pady 10 -padx 3 
# ----------------
# rearrangement
# ----------------

   label .diva.6.2.label1 -text $Dict(Xorigin) -width 20
   entry .diva.6.2.entry1 -relief sunken -width 10 -textvariable Diva2(xorig)
   label .diva.6.2.label2 -text $Dict(dx) -width 13 
   entry .diva.6.2.entry2 -relief sunken -width 10 -textvariable Diva2(dx)
   label .diva.6.2.label3 -text $Dict(Xend) -width 15 
   entry .diva.6.2.entry3 -relief sunken -width 10 -textvariable Diva2(xend)
   pack .diva.6.2.label1 .diva.6.2.entry1 .diva.6.2.label2 .diva.6.2.entry2 \
   .diva.6.2.label3 .diva.6.2.entry3   -side left

   #-------------

   label .diva.6.3.label1 -text $Dict(Yorigin) -width 20
   entry .diva.6.3.entry1 -relief sunken -width 10 -textvariable Diva2(yorig)
   label .diva.6.3.label2 -text $Dict(dy) -width 13
   entry .diva.6.3.entry2 -relief sunken -width 10 -textvariable Diva2(dy)
   label .diva.6.3.label3 -text $Dict(Yend) -width 15
   entry .diva.6.3.entry3 -relief sunken -width 10 -textvariable Diva2(yend)
   pack .diva.6.3.label1 .diva.6.3.entry1 .diva.6.3.label2 .diva.6.3.entry2 \
   .diva.6.3.label3 .diva.6.3.entry3   -side left

   #-------------

   label .diva.6.4.label -text $Dict(ExclusionValue) -width 13
   entry .diva.6.4.entry -relief sunken -width 10 -textvariable Diva2(valexcl)
   checkbutton .diva.6.4.chk -variable Diva2(SelDepth) -width 18 \
     -text $Dict(SelectedDepth) -onvalue 1 -offvalue 0
   pack .diva.6.4.label .diva.6.4.entry .diva.6.4.chk -side left -pady 10 
   pack .diva.6.4.chk -side right -pady 10 -padx 3

   #-------------

   Diva_ComputenbXY 0 

#  bind .diva.6.2.entry1 <FocusOut> {Diva_ComputenbXY}
#  bind .diva.6.2.entry2 <FocusOut> {Diva_ComputenbXY}
#  bind .diva.6.3.entry1 <FocusOut> {Diva_ComputenbXY}
#  bind .diva.6.3.entry2 <FocusOut> {Diva_ComputenbXY}
#  bind .diva.6.4.entry1 <FocusOut> {Diva_ComputenbXY}
#  bind .diva.6.4.entry2 <FocusOut> {Diva_ComputenbXY}

   bind .diva.6.2.entry1 <FocusOut> {Diva_ComputenbXY}
   bind .diva.6.2.entry2 <FocusOut> {Diva_ComputenbXY}
   bind .diva.6.2.entry3 <FocusOut> {Diva_ComputenbXY}
   bind .diva.6.3.entry1 <FocusOut> {Diva_ComputenbXY}
   bind .diva.6.3.entry2 <FocusOut> {Diva_ComputenbXY}
   bind .diva.6.3.entry3 <FocusOut> {Diva_ComputenbXY}

#--------------------------------------------------------------

  MakeFrame .diva.7 1 flat

  set Diva2(ov) $Diva2(ov) 
  label .diva.7.1.label -text  $Dict(OutputVolume)
  scale .diva.7.1.scale -orient horizontal -from 1 -to 5 -length 145 \
    -command "set Diva2(ov)"
  .diva.7.1.scale set $Diva2(ov)

  pack .diva.7.1.label -side left
  pack .diva.7.1.scale -side right

#--------------------------------------------------------------

  MakeFrame .diva.8 2 flat

  button .diva.8.1.loaddescr -text $Dict(LoadConf) -width 16 -command { 
    DivaLoadConfigFile
  }
  button .diva.8.1.savedescr -text $Dict(SaveConf) -width 16 -command {
   DivaSaveConfigFile
  }
  button .diva.8.2.ok -text "$Dict(Ok)" -width 16 -command {
    SetDivaParam
    CopyGridInfo
    CopArr Diva2 Diva
    DivaRun
    CopyAnalysisFile 
  }
  button .diva.8.2.cancel -text "$Dict(Cancel)" -width 16 -command {
   QuitMenu .diva
  }

  pack .diva.8.1.loaddescr .diva.8.1.savedescr -side left -fill x -expand 1 -padx 5 -pady 5
  pack .diva.8.2.ok .diva.8.2.cancel -side left -fill x -expand 1 -padx 5 -pady 5
  DivaSwitch2D3D
  DivaSwitchError
  DivaSwitchAnalyseParameter

#--------------------------------------------------------------

   MakeFrame .diva.9 1 flat

   button .diva.9.1.anal -text $Dict(VisuAnalysis) -relief raised\
   -width 16 -command {VisuAnalysisOutputFile}

   button .diva.9.1.error -text $Dict(VisuError) -relief raised\
   -width 16 -command {VisuErrorOutputFile}
                       
   
pack .diva.9.1.anal -side left -padx 5 -pady 5 -fill x -expand 1
   pack .diva.9.1.error -side left -padx 5 -pady 5 -fill x -expand 1

}

##############################################################################
proc DivaSwitchError {} {

 global Diva2 disable_foreground active_foreground

# if {$Diva2(error) == 0} {
#   .diva.2.11.label configure -state disabled
#   .diva.2.11.entry configure -foreground $disable_foreground
#    bind .diva.2.11.entry <FocusIn> {focus .diva}
# } else {
#   .diva.2.11.label configure -state normal
#   .diva.2.11.entry configure -foreground $active_foreground
#    bind .diva.2.11.entry <FocusIn> {focus .diva.2.11.entry}
# }
#---------------------------------
#changement des numeros des labels
#---------------------------------

if {$Diva2(error) == 0} {
   .diva.2.6.label configure -state disabled
   .diva.2.6.entry configure -foreground $disable_foreground
    bind .diva.2.6.entry <FocusIn> {focus .diva}
 } else {
   .diva.2.6.label configure -state normal
   .diva.2.6.entry configure -foreground $active_foreground
    bind .diva.2.6.entry <FocusIn> {focus .diva.2.6.entry}
 }

}
##############################################################################

proc DivaSwitchAnalyseParameter {} {

 global Diva2 disable_foreground active_foreground

 if {$Diva2(dt) == 0} {

#########################################################
#   je change les identifiants des buttons et des entry
#########################################################
#   .diva.4.2.1.label configure -state disabled
#   .diva.4.2.1.entry configure -foreground $disable_foreground
#    bind .diva.4.2.1.entry <FocusIn> {focus .diva}
#   .diva.4.2.2.label configure -state disabled
#   .diva.4.2.2.entry configure -foreground $disable_foreground
#    bind .diva.4.2.2.entry <FocusIn> {focus .diva}
#########################################################

   .diva.4.2.label configure -state disabled
   .diva.4.2.entry configure -foreground $disable_foreground
    bind .diva.4.2.entry <FocusIn> {focus .diva}
   .diva.4.3.label configure -state disabled
   .diva.4.3.entry configure -foreground $disable_foreground
    bind .diva.4.3.entry <FocusIn> {focus .diva}

#   .diva.5.2.1.label configure -state disabled
#   .diva.5.2.1.entry configure -foreground $disable_foreground
#    bind .diva.5.2.1.entry <FocusIn> {focus .diva}
   .diva.4.1.radio deselect
   .diva.4.1.radio configure -state disabled
   .diva.5.1.radio select
   .diva.3.3.label1 configure -state disabled
   .diva.3.3.entry1 configure -foreground $disable_foreground
   .diva.3.3.label2 configure -state disabled
   .diva.3.3.entry2 configure -foreground $disable_foreground
    bind .diva.3.3.entry1 <FocusIn> {focus .diva}
    bind .diva.3.3.entry2 <FocusIn> {focus .diva}
 } elseif { $Diva2(dt) == 3 } {
   .diva.3.3.label1 configure -state normal
   .diva.3.3.entry1 configure -foreground $active_foreground
    bind .diva.3.3.entry1 <FocusIn> {focus .diva.3.3.entry1}
   .diva.3.3.label2 configure -state normal
   .diva.3.3.entry2 configure -foreground $active_foreground
    bind .diva.3.3.entry2 <FocusIn> {focus .diva.3.3.entry2}
 } else {
#   .diva.4.2.1.label configure -state normal
#   .diva.4.2.1.entry configure -foreground $active_foreground
#    bind .diva.4.2.1.entry <FocusIn> {focus .diva.4.2.1.entry}
#   .diva.4.2.2.label configure -state normal
#   .diva.4.2.2.entry configure -foreground $active_foreground
#    bind .diva.4.2.2.entry <FocusIn> {focus .diva.4.2.2.entry}
##############
# changements des identifiants
##############
   .diva.4.2.label configure -state normal
   .diva.4.2.entry configure -foreground $active_foreground
    bind .diva.4.2.entry <FocusIn> {focus .diva.4.2.entry}
   .diva.4.3.label configure -state normal
   .diva.4.3.entry configure -foreground $active_foreground
    bind .diva.4.3.entry <FocusIn> {focus .diva.4.3.entry}


##########################################
#   .diva.5.2.1.label configure -state normal
#   .diva.5.2.1.entry configure -foreground $active_foreground
#    bind .diva.5.2.1.entry <FocusIn> {focus .diva.5.2.1.entry}
   .diva.4.1.radio configure -state normal
   .diva.3.3.label1 configure -state disabled
   .diva.3.3.entry1 configure -foreground $disable_foreground
    bind .diva.3.3.entry1 <FocusIn> {focus .diva}
   .diva.3.3.label2 configure -state disabled
   .diva.3.3.entry2 configure -foreground $disable_foreground
    bind .diva.3.3.entry2 <FocusIn> {focus .diva}
 }

}

##############################################################################

##############################################################################

proc DivaSwitch2D3D {} {

 global Diva2 disable_foreground active_foreground

# if {$Diva2(femdim) == 2} {
#   .diva.2.1.load configure -state normal
#   .diva.2.1.label configure -state normal
#   .diva.2.2.entry configure -foreground $active_foreground
#   .diva.2.3.load configure -state disabled
#   .diva.2.3.label configure -state disabled
#   .diva.2.4.entry configure -foreground $disable_foreground
#   .diva.2.7.load configure -state disabled
#   .diva.2.7.label configure -state disabled
#   .diva.2.8.entry configure -foreground $disable_foreground
#    bind .diva.2.2.entry <FocusIn> {focus .diva.2.2.entry}
#    bind .diva.2.4.entry <FocusIn> {focus .diva}
#    bind .diva.2.8.entry <FocusIn> {focus .diva}
# } else {
#   .diva.2.1.load configure -state disabled
#   .diva.2.1.label configure -state disabled
#   .diva.2.2.entry configure -foreground $disable_foreground
#   .diva.2.3.load configure -state normal
#   .diva.2.3.label configure -state normal
#   .diva.2.4.entry configure -foreground $active_foreground
#   .diva.2.7.load configure -state normal
#   .diva.2.7.label configure -state normal
#   .diva.2.8.entry configure -foreground $active_foreground
#    bind .diva.2.2.entry <FocusIn> {focus .diva}
#    bind .diva.2.4.entry <FocusIn> {focus .diva.2.4.entry}
#    bind .diva.2.8.entry <FocusIn> {focus .diva.2.8.entry}
# }

#---------------------------------
#changement des numeros des labels
#---------------------------------
if {$Diva2(femdim) == 2} {
   .diva.2.1.load configure -state normal
   .diva.2.1.label configure -state normal
   .diva.2.1.entry configure -foreground $active_foreground
   .diva.2.2.load configure -state disabled
   .diva.2.2.label configure -state disabled
   .diva.2.2.entry configure -foreground $disable_foreground
   .diva.2.4.load configure -state disabled
   .diva.2.4.label configure -state disabled
   .diva.2.4.entry configure -foreground $disable_foreground
    bind .diva.2.1.entry <FocusIn> {focus .diva.2.2.entry}
    bind .diva.2.2.entry <FocusIn> {focus .diva}
    bind .diva.2.4.entry <FocusIn> {focus .diva}
 } else {
   .diva.2.1.load configure -state disabled
   .diva.2.1.label configure -state disabled
   .diva.2.1.entry configure -foreground $disable_foreground
   .diva.2.2.load configure -state normal
   .diva.2.2.label configure -state normal
   .diva.2.2.entry configure -foreground $active_foreground
   .diva.2.4.load configure -state normal
   .diva.2.4.label configure -state normal
   .diva.2.4.entry configure -foreground $active_foreground
    bind .diva.2.1.entry <FocusIn> {focus .diva}
    bind .diva.2.2.entry <FocusIn> {focus .diva.2.1.entry}
    bind .diva.2.4.entry <FocusIn> {focus .diva.2.4.entry}
 }

}

##############################################################################

proc DivaSaveConfigFile {} {

  global Diva2 Fs Dir DivaConfFile NameDiva

  set NameDiva(1) {femdim}
  set NameDiva(2) {DataName}
  set NameDiva(3) {DepthName}
  set NameDiva(4) {MeshName}
  set NameDiva(5) {BathName}
  set NameDiva(6) {OutName}
  set NameDiva(7) {error}
  set NameDiva(8) {Varbak}
  set NameDiva(9) {dt} 
  set NameDiva(10) {RBTcl}
  set NameDiva(11) {clref}
  set NameDiva(12) {muref}
  set NameDiva(13) {cl}
  set NameDiva(14) {snr}
  set NameDiva(15) {alph0}
  set NameDiva(16) {alpha1}
  set NameDiva(17) {mu}
  set NameDiva(18) {xorig} 
  set NameDiva(19) {yorig}
  set NameDiva(20) {dx}
  set NameDiva(21) {dy}
  set NameDiva(22) {xend}
  set NameDiva(23) {yend}
  set NameDiva(24) {valexcl}
  set NameDiva(25) {SelDepth}
  set NameDiva(26) {ov}
  set NameDiva(27) {femdim}

 set fname [tk_getSaveFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -title {Save as...}
  ]

  set FileID [open $fname w+]

  for {set x 1} {$x<28} {incr x} {
    puts $FileID $Diva2($NameDiva($x)) 
    }

  close $FileID
  return
}

##############################################################################

proc DivaLoadConfigFile {} {

  global Diva2 Fs Dir DivaConfFile NameDiva
  
  set NameDiva(1) {femdim}
  set NameDiva(2) {DataName}
  set NameDiva(3) {DepthName}
  set NameDiva(4) {MeshName}
  set NameDiva(5) {BathName}
  set NameDiva(6) {OutName}
  set NameDiva(7) {error}
  set NameDiva(8) {Varbak}
  set NameDiva(9) {dt} 
  set NameDiva(10) {RBTcl}
  set NameDiva(11) {clref}
  set NameDiva(12) {muref}
  set NameDiva(13) {cl}
  set NameDiva(14) {snr}
  set NameDiva(15) {alph0}
  set NameDiva(16) {alpha1}
  set NameDiva(17) {mu}
  set NameDiva(18) {xorig} 
  set NameDiva(19) {yorig}
  set NameDiva(20) {dx}
  set NameDiva(21) {dy}
  set NameDiva(22) {xend}
  set NameDiva(23) {yend}
  set NameDiva(24) {valexcl}
  set NameDiva(25) {SelDepth}
  set NameDiva(26) {ov}
  set NameDiva(27) {femdim}



  set DivaConfFile [tk_getOpenFile\
  -defaultextension .conf\
  -initialdir $Fs(data_conf)\
  -initialfile adriatic_analysis.conf\
  -title {Save as...}
  ]
  
  set filename [open $DivaConfFile]
  set lineNumber 0
  set i 1
  while {[gets $filename line] >= 0} {
  set Diva2($NameDiva($i)) $line
  incr i 1
  }
  
  DivaSwitch2D3D 
}


################################################################################

proc SetDivaParam {} {

  global Diva2

  if { ($Diva2(cl) != "") && ($Diva2(snr) != "") && ($Diva2(RBTcl) == 1) } {
    set Diva2(alph0) [expr 1./($Diva2(cl)*$Diva2(cl)*$Diva2(cl)*$Diva2(cl))]
    set Diva2(alph1) [expr 2./($Diva2(cl)*$Diva2(cl))]
    set Diva2(mu) [expr $Diva2(snr)*4.*3.14159265/($Diva2(cl)*$Diva2(cl))]
  }

}


################################################################################
proc DivaRun {{w .divarun}} {
  global Diva Dir Dict Trash

  if { $Diva(dt) == 3 } {
   if { [DivaRunBis -1] == 0 } {
    if [catch {exec mv "$Diva(OutName)" "$Diva(OutName).ref" 2> $Trash} err] {
       ErrorMessageBox $Dict(ErrorInMv) $err
       return
    }
    if { [DivaRunBis 1] == 0 } {
      if [catch {exec mv "$Diva(OutName)"  "$Diva(OutName).incr" 2> $Trash} err] {
       ErrorMessageBox $Dict(ErrorInMv) $err
       return
    }
      set fsum [open |$Diva(sum_exec) a]
      puts $fsum "$Diva(OutName).ref"
      puts $fsum "$Diva(OutName).incr"
      puts $fsum "$Diva(OutName)"
      if [catch {close $fsum} err] {
       ErrorMessageBox $Dict(ErrorInSumRef) $err
       return
      }
    }
   }
  } else {
   DivaRunBis 0 
  }
}
################################################################################
proc DivaRunBis { seminormed } {

  global Diva Dir Dict

###################  2D CASE  #################
  if {$Diva(femdim) == 2} {

#     
#   Test pour nombre donnees >= 3
#
   
    if { $seminormed == 1} {
     set datafile $Diva(OutName).data.incr
    } else {
     set datafile $Diva(DataName)
    }
 
    set count 0
    set fileid [open $datafile r]
    while { [gets $fileid line] > 0 && $count <= 3} {incr count}
    if {$count <3 && $Diva(dt)==2} {
      ErrorMessageBox Error $Dict(NumberOfDataMustBeGreaterThan3)
      return -1
    }
    close $fileid

    if [catch {DivaPre $datafile $Diva(OutName) $seminormed} errtmp] {
      ErrorMessageBox $Dict(ErrorProcessingDivaInputFiles) $errtmp 
      DivaPost $Diva(OutName) $seminormed
      return -1
    }
    set curdir [pwd]
    cd $Dir(temp)

    set fdiva [open |$Diva(diva_exec) r]
    while { [gets $fdiva line] >= 0 } {
      puts $line
    }
    if [catch {close $fdiva} result] {
      if { $result != "STOP " } {
        ErrorMessageBox $Dict(ErrorInDiva) $result 
      }
    }
    DivaPost $Diva(OutName) $seminormed

    if { $seminormed == -1 } {
     set fsubstref [open |$Diva(substref_exec) a]
     puts $fsubstref $Diva(DataName)
     puts $fsubstref $Diva(OutName).data.ref
     puts $fsubstref $Diva(OutName).data.incr 
     if [catch {close $fsubstref} result] {
      ErrorMessageBox $Dict(ErrorInSubstref) $result
      puts $Dict(ErrorInSubstref)
      return -1
     }
    }

    cd $curdir

#
# Cree le fichier .info
#
    if { $seminormed >= 0 } {
     DivaInfo
    }
    return 0
###################  3D CASE  #################

  } else {

    set Diva(outname_rel) [RelName $Diva(OutName)]
    set Diva(errlist) {}
    LoadDepthList Diva(DepthName) depthlist_tmp datanamelist_tmp
    puts $datanamelist_tmp
    puts $depthlist_tmp
#
# Eliminer les fichiers ou nombre de donnees < 3
#

    set index 0
    set Diva(datanamelist) {}
    set Diva(depthlist) {}
    foreach arg $datanamelist_tmp {
      set fileid [open $arg r]
      set count 0
      while {[gets $fileid line] > 0 && $count <= 3} {incr count}
      if {$count >= 3} {
        lappend Diva(datanamelist) [lindex $datanamelist_tmp $index]
        lappend Diva(depthlist) [lindex $depthlist_tmp $index]
      }
      close $fileid
      incr index
    }
      
    if {$Diva(datanamelist) == {} && $Diva(dt)==2 } {
      ErrorMessageBox Error $Dict(NumberOfDataMustBeGreaterThan3)
      return -1
    }

    if {$Diva(SelDepth)} {
      set ind [SelectDepth Diva(depthlist)]
      if {$ind != -1} {
        set Diva(depthlist) [lindex $Diva(depthlist) $ind]
        set Diva(datanamelist) [lindex $Diva(datanamelist) $ind]
      } else {
        return -1
      }
    }

    set Diva(nbprof) [llength $Diva(depthlist)]

    for {set Diva(depthcount) 0} {$Diva(depthcount) < $Diva(nbprof)} {incr \
    Diva(depthcount) 1} {

      set depth [lsort [lindex $Diva(depthlist) $Diva(depthcount)]]
      if { $seminormed == 1} {
       set name $Dir(temp)/$Diva(outname_rel).$depth.data.incr
      } else {
       set name [lindex $Diva(datanamelist) $Diva(depthcount)]
      }
 
      if [catch {DivaPre $name $Diva(OutName).$depth $seminormed} errtmp] {
        ErrorMessageBox $Dict(ErrorProcessingDivaInputFiles) $errtmp 
        DivaPost $Diva(OutName).$depth $seminormed 
        return -1
      }

      set curdir [pwd]
      cd $Dir(temp)

      set fdiva [open |$Diva(diva_exec) r]
      while { [gets $fdiva line] >= 0 } {
        puts $line
      }
      if [catch {close $fdiva} result] {
        if { $result != "STOP " } {
          cd $curdir
          ErrorMessageBox "Error in Diva (depth $depth)!" $result 
          lappend Diva(errlist) $depth
          DivaPost $Diva(OutName).$depth $seminormed
          return -1
        }
      }

      DivaPost $Dir(temp)/$Diva(outname_rel).$depth $seminormed

      if { $seminormed == -1 } {
       set fsubstref [open |$Diva(substref_exec) a]
       puts $fsubstref $name
       puts $fsubstref $Dir(temp)/$Diva(outname_rel).$depth.data.ref 
       puts $fsubstref $Dir(temp)/$Diva(outname_rel).$depth.data.incr 
       if [catch {close $fsubstref} result] {
        ErrorMessageBox $Dict(ErrorInSubstref) $result
        puts $Dict(ErrorInSubstref)
        return -1
       }
      }

      cd $curdir
      
    }

#   Cree le fichier resultat 3D a partir des fichiers 2D

#   set fconcat stdout 
    set fconcat [open |$Diva(concat_exec) a]

    puts $fconcat [Diva_ComputenbXY "x"]
    puts $fconcat [Diva_ComputenbXY "y"] 
    puts $fconcat $Diva(nbprof)
    puts $fconcat $Diva(OutName).tmp
    foreach depth $Diva(depthlist) {
      set d [lsort $depth]
      puts $fconcat $Dir(temp)/$Diva(outname_rel).$d
    }
    if [catch {close $fconcat} result] {
      ErrorMessageBox $Dict(ErrorWhileCreating3DFiles) $result 
      puts $Dict(ErrorWhileCreating3DFiles)
      return -1
    }

    if { $Diva(error) != 0 } {
     if { $seminormed >= 0 } {
      set fconcat [open |$Diva(concat_exec) a]

      puts $fconcat [Diva_ComputenbXY "x"]
      puts $fconcat [Diva_ComputenbXY "y"] 
      puts $fconcat $Diva(nbprof)
      puts $fconcat $Diva(OutName).tmp.error
      foreach depth $Diva(depthlist) {
        set d [lsort $depth]
        puts $fconcat $Dir(temp)/$Diva(outname_rel).$d.error
      }
      if [catch {close $fconcat} result] {
        ErrorMessageBox $Dict(ErrorWhileCreating3DFiles) $result 
        puts $Dict(ErrorWhileCreating3DFiles)
        return -1
      }
     }
    }

# Cree le fichier .info

    DivaInfo

#   cree le fichier resultat 3D masque par la bathymetrie

    set fmask [open |$Diva(mask_exec) a]
    puts $fmask $Diva(BathName)
    puts $fmask $Diva(OutName).tmp
    puts $fmask $Diva(BathName).info
    puts $fmask $Diva(OutName).info
    puts $fmask $Diva(OutName)
    if [catch {close $fmask} result] {
      ErrorMessageBox $Dict(ErrorWhileCreating3DMaskedFiles) $result
	puts $Dict(ErrorWhileCreating3DMaskedFiles)
	puts $result
      return -1
    }

    if { $Diva(error) != 0 } {
     if { $seminormed >= 0 } {
      set fmask [open |$Diva(mask_exec) a]
      puts $fmask $Diva(BathName)
      puts $fmask $Diva(OutName).tmp.error
      puts $fmask $Diva(BathName).info
      puts $fmask $Diva(OutName).info
      puts $fmask $Diva(OutName).error
      if [catch {close $fmask} result] {
        ErrorMessageBox $Dict(ErrorWhileCreating3DMaskedFiles) $result
  	puts $Dict(ErrorWhileCreating3DMaskedFiles)
	puts $result
        return -1
      }
     }
    }

#
#   Efface les fichiers 2D
#

    foreach depth $Diva(depthlist) {
      set d [lsort $depth]
      catch {exec rm "Dir(temp)/$Diva(outname_rel).$d" 2> $Trash}
      if { $Diva(error) != 0 } {
       if { $seminormed >= 0 } {
        catch {exec rm "Dir(temp)/$Diva(outname_rel).$d.error" 2> $Trash}
       }
      } 
    }
  }   
  return 0
}

################################################################################

proc DivaInfo {} {

  global Diva Trash

# Cree le fichier .info

  set finfo [open $Diva(OutName).info w]
  puts $finfo $Diva(xorig)
  puts $finfo $Diva(xend) 
  puts $finfo $Diva(yorig)
  puts $finfo $Diva(yend) 
  if {$Diva(femdim) == 2} {
    puts $finfo "0"
  } else {
    puts $finfo $Diva(nbprof)
    foreach depth $Diva(depthlist) {
      puts $finfo [lsort $depth]
    }
  }
  close $finfo
  if { $Diva(error) != 0 } {
    catch {exec cp $Diva(OutName).info $Diva(OutName).error.info 2> $Trash}
  }

}

################################################################################


proc DivaPre { dataname outname seminormed} {

  global Diva Dir Fem Coordinates Dict

  if { $Diva(RBTcl) } SetDivaParam

# Genere rigidite (fort.60) si 3D

  if {$Diva(femdim) == 3} {
    set fstiff [open |$Diva(stiff_exec) w]
    puts $fstiff $Diva(MeshName).mh4
    puts $fstiff $Diva(MeshName).mh5
    puts $fstiff $Dir(temp)/fort.60
    puts $fstiff [expr $Diva(depthcount)+1] 
    close $fstiff
  }

# Lire info dans .mh4

  set file_18 $Diva(MeshName).mh4
  set f18id [open $file_18 r] 
  gets $f18id Diva(nov)
  gets $f18id Diva(noi)
  gets $f18id Diva(noe)
  close $f18id

# Verifier si .ptl existe

#   if [file exists "$Diva(MeshName).ptl"] then {
#    set Diva(ispec) 2
#   } else {
#    set Diva(ispec) 1
#   }
  if { $seminormed >= 0 } {
   set Diva(ispec) 1
  } else {
   set Diva(ispec) 3
  } 

# Generer le fichier .drv

  set outname_short [RelName $outname]

  set file_10 "$Dir(temp)/$outname_short.drv"
  set f10id [open $file_10 w]
  puts $f10id "coord $Diva(ov)"
  puts $f10id "$Coordinates(system)"
  puts $f10id "mathpr $Diva(ov)"
  puts $f10id "2  -> ITYP"
  puts $f10id "0  -> ISYM"
  puts $f10id "2  -> IPB"
  puts $f10id "topolo $Diva(ov)"
  puts $f10id "$Diva(nov) -> Number of Vertex Nodes"
  puts $f10id "$Diva(noi) -> Number of Interface Nodes"
  puts $f10id "$Diva(noe) -> Number of Elments"
  puts $f10id "datapr $Diva(ov)"
  puts $f10id "$Diva(dt)"
  puts $f10id "solver $Diva(ov)"
  if {$Diva(femdim) == 2} {
    puts $f10id "0"
  } else {
    puts $f10id "1"
  }
  puts $f10id "stores $Diva(ov)"
  puts $f10id "$Diva(ispec)  -> 1 if normal or seminormed final, otherwise 3"
  if { $Diva(error) != 0 } {
   if { $seminormed >= 0 } {
    puts $f10id "esterr $Diva(ov)"
#    puts $f10id "esterr 2"
   }
  }
  puts $f10id "stopex"
  close $f10id

# Generer le fichier .wei

  set file_12 "$Dir(temp)/$outname_short.wei"
  set f12id [open $file_12 w]
  if { $seminormed == -1 } {
   set alpha0 0
   set alpha1 [expr 2./($Diva(clref)*$Diva(clref))]
  } else {
   set alpha0 $Diva(alph0)
   set alpha1 $Diva(alph1)
  }
  puts $f12id "$alpha0  -> alpha0"
  puts $f12id "$alpha1  -> alpha1"
  close $f12id


# Generer le fichier .grd

  set file_13 "$Dir(temp)/$outname_short.grd"
  set f13id [open $file_13 w]
  set xorig_tmp [expr $Diva(xorig) - ($Diva(dx))]
  set yorig_tmp [expr $Diva(yorig) - ($Diva(dy))]
  puts $f13id "$xorig_tmp  $yorig_tmp  -> xorigin, yorigin"
  puts $f13id "$Diva(dx)  $Diva(dy)  -> dx, dy"
  set Diva(nbx) [Diva_ComputenbXY "x"]
  set Diva(nby) [Diva_ComputenbXY "y"] 
  puts $f13id "$Diva(nbx)  $Diva(nby)  -> nx, ny"
  puts $f13id "$Diva(valexcl)  -> Exclusion Value"
  close $f13id

# Generer le fichier .mu

  set file_21 "$Dir(temp)/$outname_short.mu"
  set f21id [open $file_21 w]
  if { $seminormed == -1 } {
   set mu $Diva(muref)
  } else {
   set mu $Diva(mu)
  } 
  puts $f21id "$mu  -> mu"
  close $f21id

# Genere fort.20 en copiant .dat avec .mu
# Modification (nov 2006): .mu is added ONLY IF
# the .dat file contains 3 columns
 
  set file_20 $dataname

  set f20id [open $file_20 r]
  set file_20bis $Dir(temp)/fort.20
  set f20bisid [open $file_20bis w]
  

  #while { [set c [gets $f20id val]] != -1 } {
  #  puts $f20bisid [append val "       $mu"]
  #} 
  
  ####################
  # added November 06
  ####################
  
  # counts the number of elements per line
  gets $f20id line
  set ncol [ llength ($line)]
  set lineNumber 0

  # writes the first line
  if {$ncol == 3} {
  set line [append line " $mu"]
  puts $f20bisid $line
  } else {
  set linelast [lindex $line end]
  set linelast [expr $linelast*$mu]
  set line [lreplace $line end end $linelast]
  puts $f20bisid $line
  }

  # loop to write the remaining lines
  if {$ncol == 3} {
  set lineNumber 0
  while {[gets $f20id line] != -1} {
  set line [append line " $mu"]
  puts $f20bisid $line
  }
  } else {
  set lineNumber 0
  while {[gets $f20id line] != -1} {
  set linelast [lindex $line end]
  set linelast [expr $linelast*$mu]
  set line [lreplace $line end end $linelast]
  puts $f20bisid $line
  }
  }

  # closes the files

  close $f20id 
  close $f20bisid 

  # --------------------




  if { $seminormed == -1 } {
   catch {exec cp $Dir(temp)/fort.20 $Dir(temp)/fort.79 }
  }

# Si calcul de l'erreur requis, construire fort.15
  
  if { $Diva(error) != 0 } {
   if { $seminormed >= 0 } {
    set file_15 $Dir(temp)/fort.15
    set f15id [open $file_15 w]
    puts $f15id $Diva(Varbak)
    close $f15id 
   } 
  }

# Copie vers les unites fortran

  exec cp $file_10 $Dir(temp)/fort.10
  exec cp $Diva(MeshName) $Dir(temp)/fort.11
  exec cp $file_12 $Dir(temp)/fort.12
  exec cp $file_13 $Dir(temp)/fort.13

}

################################################################################

proc DivaPost { outname seminormed } {

  global Dir Coordinates Dict Diva Trash

#  foreach file {10 11 12 13 14 15 20 22 30 50 60 79 79.pre 80 81 83} {
#    catch {exec rm "$Dir(temp)/fort.$file" 2> $Trash}
#  }
  if {$outname != 0} {
#    exec mv "$Dir(temp)/fort.82" $outname.data.ref 2> $Trash
#    exec mv "$Dir(temp)/fort.84" $outname 2> $Trash
#    exec mv "$Dir(temp)/fort.87" $outname.error 2> $Trash
    catch {exec mv "$Dir(temp)/fort.84" $outname 2> $Trash}
    catch {exec mv "$Dir(temp)/fort.87" $outname.error 2> $Trash}
    catch {exec mv "$Dir(temp)/fort.82" $outname.data.ref 2> $Trash}
    catch {exec rm "$Dir(temp)/$(outname).drv" 2> $Trash}
    catch {exec rm "$Dir(temp)/$(outname).mu" 2> $Trash}
    catch {exec rm "$Dir(temp)/$(outname).wei" 2> $Trash}
  }

}

################################################################################

#proc Diva_LoadData {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DataFileName) -full 0 \
#    -dir $Dir(data) -filter $Fs(diva_data_filter)
#  
#  if {[.fs activate]} {
#    set Diva2(DataName) [.fs get]
#    set Fs(diva_data) [PathName $Diva2(DataName)]
#  }
#  .fs delete 
#  return
#}

proc Diva_LoadData {} {

global Diva2 Fs Dir Dict

set Diva2(DataName) [
tk_getOpenFile -defaultextension .data\
  -initialdir $Dir(data) \
  -title $Dict(DataFileName)
]
return
}


################################################################################

#proc Diva_LoadRef {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(RefFileName) -full 0 \
#    -dir $Dir(data) -filter $Fs(diva_data_filter)
#  
#  if {[.fs activate]} {
#    set Diva2(RefName) [.fs get]
#    set Fs(diva_refdata) [PathName $Diva2(DataName)]
#  }
#  .fs delete 
#  return
#
#}

proc Diva_LoadRef  {} {

global Diva2 Fs Dir Dict

set Diva2(RefName) [
tk_getOpenFile -defaultextension .data\
  -initialdir $Dir(data) \
  -title $Dict(RefFileName)
]
set Fs(diva_refdata) [PathName $Diva2(DataName)]

return
}

################################################################################

#proc Diva_LoadDepthData {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(DepthFileName) -full 0 \
#    -dir $Dir(depth) -filter $Fs(diva_data_depth_filter)
# 
#  if {[.fs activate]} {
#    set Diva2(DepthName) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Diva_LoadDepthData {} {

global Diva2 Fs Dir Dict

set Diva2(RefName) [
tk_getOpenFile -defaultextension .depth\
  -initialdir $Dir(depth) \
  -title $Dict(DepthFileName)
]
return
}


################################################################################

#proc Diva_LoadMesh {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(MeshFileName) -filter $Fs(diva_mesh_filter) \
#    -full 0 -dir $Dir(mesh)
#
#  if {[.fs activate]} {
#    set Diva2(MeshName) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Diva_LoadMesh {} {

global Diva2 Fs Dir Dict

set Diva2(MeshName) [
tk_getOpenFile -defaultextension .mesh\
  -initialdir $Dir(mesh) \
  -title $Dict(MeshFileName)
]
return
}

################################################################################

#proc Diva_LoadBath {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(BathymetryFileName) -full 0 \
#    -dir $Dir(bathymetry) -filter $Fs(diva_bat_filter) 
#
#  if {[.fs activate]} {
#    set Diva2(BathName) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Diva_LoadBath {} {

global Diva2 Fs Dir Dict

set Diva2(BathName) [
tk_getOpenFile -defaultextension .bat\
  -initialdir $Dir(bathymetry) \
  -title $Dict(BathymetryFileName)
]
return
}

################################################################################

#proc Diva_LoadOut {} {
#
#  global Diva2 Fs Dir Dict
#
#  DivaFileSelect .fs -title $Dict(AnalysisOutputFileName) -full 0 \
#    -dir $Dir(analysis) -filter $Fs(diva_res_filter)
#
#  if {[.fs activate]} {
#    set Diva2(OutName) [.fs get]
#  }
#  .fs delete
#  return
#
#}

proc Diva_LoadOut {} {

global Diva2 Fs Dir Dict

set Diva2(OutName) [
tk_getSaveFile -defaultextension .anl\
  -initialdir $Dir(analysis) \
  -title $Dict(AnalysisOutputFileName)
]
return
}

################################################################################

proc Diva_ComputenbXY { { xy 0 } } {

  global Diva2 Dict

  if {![Empty $Diva2(xorig)] && ![Empty $Diva2(dx)] && ![Empty $Diva2(xend)] && \
      ![Empty $Diva2(yorig)] && ![Empty $Diva2(dy)] && ![Empty $Diva2(yend)]} {

    set Diva2(nbx) [expr ($Diva2(xend) - ($Diva2(xorig)) )/$Diva2(dx)]
    set Diva2(nby) [expr ($Diva2(yend) - ($Diva2(yorig)) )/$Diva2(dy)] 
    set Diva2(nbx) [expr round($Diva2(nbx))+1]
    set Diva2(nby) [expr round($Diva2(nby))+1]
    .diva.6.1.title config -text "$Dict(OutputGrid) (Nb_x = $Diva2(nbx) ,\
      Nb_y = $Diva2(nby))"

  }
  if { $xy == "x" } { return $Diva2(nbx) 
  } elseif { $xy == "y" } { return $Diva2(nby) }
}


################################################################################
# copie les informations sur la grille: x/y origine, dx/y, x/y end
# qui serviront pour la création de la sortie en format NetCDF
################################################################################


proc CopyGridInfo {} {

global Diva2 Dir



 set GridId [open $Dir(analysis)/GridInfo.dat w]

 #set info1 parameter(xorig=$Diva2(xorig))
 #set info2 parameter(yorig=$Diva2(yorig))
 #set info3 parameter(dx=$Diva2(dx))
 #set info4 parameter(dy=$Diva2(dy))
 #set info5 parameter(xend=$Diva2(xend))
 #set info6 parameter(yend=$Diva2(yend))

 puts $GridId $Diva2(xorig)
 puts $GridId $Diva2(yorig)
 puts $GridId $Diva2(dx)
 puts $GridId $Diva2(dy)
 puts $GridId $Diva2(xend)
 puts $GridId $Diva2(yend)



 close $GridId 

}

# -------------------------------------------------------------------

proc VisuAnalysisOutputFile {} {
global Dict filename NameAnalysis w

#set filename [
#tk_getOpenFile -defaultextension .anl0\
#  -initialdir $Dir(analysis) \
#  -initialfile $Diva2(OutName)\
#  -title Dict(OpenAnalysis)
#]
#

set filename $NameAnalysis.anl0
set fileid [open $filename] 
set full_text [read $fileid]

set w .text
catch {destroy $w}
toplevel $w
wm title $w $Dict(OpenAnalysisFile)
wm iconname $w "text"

label $w.label -text $filename 
pack $w.label -side top -fill x
text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set"\
-setgrid 1 -height 30
scrollbar $w.scroll -command "$w.text yview"

pack $w.scroll -side right -fill y
pack $w.text -expand yes -fill both

$w.text insert 0.0 $full_text 
button $w.button -text $Dict(Close) -command {destroy $w} -width 110 -relief raised
pack $w.button -expand 1 -fill x
}


# -------------------------------------------------------------------

proc VisuErrorOutputFile {} {
global Dict filename NameAnalysis w

#set filename [
#tk_getOpenFile -defaultextension .error0\
#  -initialdir $Dir(analysis) \
#  -initialfile $Diva2(OutName)\
#  -title Dict(OpenAnalysis)
#]


set filename $NameAnalysis.error0
set fileid [open $filename] 

set full_text [read $fileid]

set w .text
catch {destroy $w}
toplevel $w
wm title $w $Dict(OpenErrorFile)
wm iconname $w "text"

label $w.label -text $filename 
pack $w.label -side top -fill x
text $w.text -relief sunken -bd 2 -yscrollcommand "$w.scroll set"\
-setgrid 1 -height 30
scrollbar $w.scroll -command "$w.text yview"

pack $w.scroll -side right -fill y
pack $w.text -expand yes -fill both

$w.text insert 0.0 $full_text 
button $w.button -text $Dict(Close) -command {destroy $w} -width 110 -relief raised
pack $w.button -expand 1 -fill x
}


# -------------------------------------------------------------------

proc CopyAnalysisFile {} {

global Dir Diva NameAnalysis 

 set NameAnalysis [CutString first $Diva(OutName) 4] 

 exec cp $Dir(temp)/fort.83 $NameAnalysis.anl0
 exec cp $Dir(temp)/fort.84 $NameAnalysis.anl
 exec cp $Dir(temp)/fort.86 $NameAnalysis.error0 
 exec cp $Dir(temp)/fort.87 $NameAnalysis.error 
 exec cp $Dir(temp)/fort.82 $NameAnalysis.data.ref 

}

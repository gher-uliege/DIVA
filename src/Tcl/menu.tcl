#######################################################
#######################################################
# Root window menu bar
#######################################################
#######################################################
proc DivaMenuBar {} {

global Dict

frame .menubar -bg grey 

pack .menubar -side top -fill x

#######################################################
# Create Menu Bar Buttons
#######################################################

#######################################################
# File Menu 
#######################################################

menubutton .menubar.file -text $Dict(File) -underline 0 -menu .menubar.file.menu

set m .menubar.file.menu

menu $m
	$m add command -label $Dict(Data) -accelerator "Ctrl+d" -underline 0 -command "p_data1"
        $m add command -label $Dict(Mesh) -accelerator "Ctrl+m" -underline 0 -command p_fem1
	$m add command -label $Dict(Analyse) -accelerator "Ctrl+a" -underline 1 -command p_diva
        $m add separator
	$m add command -label $Dict(Quit) -accelerator "Ctrl+q" -underline 0 -command p_quit

bind Entry <Control-x> p_cut
bind Entry <Control-c> p_copy
bind Entry <Control-v> p_paste

#######################################################
# Edit Menu
#######################################################

#menubutton .menubar.edit -text $Dict(Edit) -underline 0 -menu .menubar.edit.menu
#
#set m .menubar.edit.menu
#
#menu $m
#	$m add command -label $Dict(Cut) -accelerator "Ctrl+x" -underline 1 -command p_cut
#        $m add command -label $Dict(Copy) -accelerator "Ctrl+c" -underline 0 -command p_copy
#        $m add command -label $Dict(Paste) -accelerator "Ctrl+v" -underline 0 -command p_paste

#######################################################
# View Menu
#######################################################
menubutton .menubar.view -text $Dict(View) -underline 0 -menu .menubar.view.menu

set m .menubar.view.menu

menu $m
        $m add command -label $Dict(Gr_Data) -accelerator "Alt+d" -underline 0 -command p_view1
        $m add command -label $Dict(Gr_Mesh) -accelerator "Alt+m" -underline 0 -command p_view2
        $m add command -label $Dict(Gr_Analyse) -accelerator "Alt+a" -underline 0 -command p_view3


#######################################################
# Options Menu
#######################################################
menubutton .menubar.options -text $Dict(Options) -underline 0 -menu .menubar.options.menu
menubutton .menubar.preference -text $Dict(Options) -underline 0 -menu .menubar.preference.menu

set m .menubar.options.menu

menu $m
  $m add command -label $Dict(Coordinates) -underline 0 -command p_options1
  $m add command -label $Dict(Directories) -underline 0 -command p_options2
  $m add command -label $Dict(Preferences) -underline 0 -command p_options3

#######################################################
# Help setting 
#######################################################

menubutton .menubar.help -text $Dict(Help) -underline 0 -menu .menubar.help.menu

set m .menubar.help.menu

menu $m 
 $m add command -label $Dict(About) -command "p_help_about"
 $m add command -label $Dict(General) -command "p_help general"
 $m add cascade -label $Dict(File) -menu $m.file
 $m add cascade -label $Dict(View) -menu $m.view
 $m add cascade -label $Dict(Options) -menu $m.options

#pack .menubar.file .menubar.edit .menubar.view .menubar.options -side left -ipadx 10
pack .menubar.file .menubar.view .menubar.options -side left -ipadx 10
pack .menubar.help -side right -ipadx 10

#SetButtonSizes .menu 1

menu $m.about

menu $m.file
 $m.file add cascade -label $Dict(Data) -menu $m.file.data
 $m.file add cascade -label $Dict(Mesh) -menu $m.file.mesh
 $m.file add cascade -label $Dict(Analyse) -menu $m.file.analyse

menu $m.file.data
 $m.file.data add command -label $Dict(DBDescriptionFileName) -command "p_help data DBDESCR"
 $m.file.data add command -label $Dict(GenericDataOutputFileName) -command "p_help data OUTGENNAME"
 $m.file.data add command -label $Dict(Years) -command "p_help data PERIOD"
 $m.file.data add command -label $Dict(Period) -command "p_help data PERIOD"
 $m.file.data add command -label $Dict(Depth) -command "p_help data DEPTH"
 $m.file.data add command -label $Dict(ContourDescriptionFile) -command "p_help data POSITION"
 $m.file.data add command -label $Dict(Position) -command "p_help 
data POSITION"
 $m.file.data add command -label $Dict(QualityCriterion) -command "p_help data FLAG"
 $m.file.data add command -label $Dict(LoadSaveConf) -command "p_help data CONF"

menu $m.file.mesh
  $m.file.mesh add command -label $Dict(2DOr3DMesh) -command "p_help fem 2D3D"
  $m.file.mesh add command -label $Dict(ContourDescriptionFile) -command "p_help fem CONTOUR"
  $m.file.mesh add command -label $Dict(GenericMeshOuputFileName) -command "p_help fem OUTMESHNAME"
  $m.file.mesh add command -label $Dict(UniformNonUniformMesh) -command "p_help fem UNIFMESH"
  $m.file.mesh add command -label $Dict(ReferenceLength) -command "p_help fem REFLENGTH"
  $m.file.mesh add command -label $Dict(SurfaceCoef) -command "p_help fem SURFREF"
  $m.file.mesh add command -label $Dict(SmoothNumber) -command "p_help fem NBSMOOTH"
  $m.file.mesh add command -label $Dict(LoadSaveConf) -command "p_help fem CONF"

menu $m.file.analyse
  $m.file.analyse add command -label $Dict(2DOr3DAnalyse) -command "p_help diva 2D3D"
  $m.file.analyse add command -label $Dict(DataFileName) -command "p_help diva DATAFNAME"
  $m.file.analyse add command -label $Dict(MeshFileName) -command "p_help diva MESHFNAME"
  $m.file.analyse add command -label $Dict(AnalysisOutputFileName) -command "p_help diva OUTNAME"
  $m.file.analyse add command -label $Dict(ComputeError) -command "p_help diva ERROR"
  $m.file.analyse add command -label $Dict(ReferenceField) -command "p_help diva DATATR"
  $m.file.analyse add command -label $Dict(CaracteristicLengthSNRatio) -command "p_help diva CLSN"
  $m.file.analyse add command -label $Dict(AlphaMu) -command "p_help diva ALPHA"
  $m.file.analyse add command -label $Dict(OutputGrid) -command "p_help diva OUTGRID"
  $m.file.analyse add command -label $Dict(OutputVolume) -command "p_help diva OUTVOL"
  $m.file.analyse add command -label $Dict(LoadSaveConf) -command "p_help diva CONF"


menu $m.view
 $m.view add command -label $Dict(Data) -command "p_help view DATA"
 $m.view add command -label $Dict(Mesh) -command "p_help view MESH"
 $m.view add command -label $Dict(Analyse) -command "p_help view DIVA"


menu $m.options
 $m.options add command -label $Dict(Coordinates) -command "p_help option COORDINATES"
 $m.options add command -label $Dict(Directories) -command "p_help option DIRECTORIES"
 $m.options add command -label $Dict(Preferences) -command "p_help option PREFERENCES"

#tk_menuBar .menubar .menubar.file .menubar.edit .menubar.view .menubar.options .menubar.help
tk_menuBar .menubar .menubar.file .menubar.view .menubar.options .menubar.help
focus .menubar
}

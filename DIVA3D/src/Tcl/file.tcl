 toplevel .options1
wm title .options1 "Coord system"
wm geometry .options1 500x500

proc MakeFrame { w n {rel raised} {bord 2}} {
  for {set i 1} {$i <= $n} {incr i 1} {
    frame $w.$i -relief $rel -bd $bord
    pack $w.$i -side top -anchor nw -fill x
  }
}

 MakeFrame .options1 4 raised

  label .options1.1.label -text "Coordinate syst" -width 30
  pack .options1.1.label

  radiobutton .options1.2.radio -text "Angular"  -relief flat \
    -variable Coordinates2(system) -value 0 \
    -command {DivaSwitchCoordinateSystem}
  radiobutton .options1.3.radio -text "Cartesian"  -relief flat \
    -variable Coordinates2(system) -value 1 \
    -command {DivaSwitchCoordinateSystem}
 
   pack .options1.2.radio .options1.3.radio -side left


  button .options1.4.ok -text "Ok" -width 16 -command {
   CopArr Coordinates2 Coordinates
   QuitMenu .options 1
  }
  button .options1.4.cancel -text "Cancel" -width 16 -command {
   QuitMenu .options 1
  }

  pack .options1.4.ok -side left
  pack .options1.4.cancel -side right



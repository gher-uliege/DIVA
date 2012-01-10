proc EntryMoveCursor {w inc} {
       set x [expr "[$w index insert] + $inc"]
       if {$x == -1} then {set x 0}
       if {$x >= [$w index end]} then {set x end}
       $w icursor $x
}

proc EntryTabChange {w depl} {
     global tab
     set win [string range $w 1 end]
     set win [string range $win 0 [expr [string first . $win] -1] ]
     switch $win {
       data1 {set tlist $tab(Data)} \
       fem1 {set tlist $tab(Fem)} \
       diva {set tlist $tab(Diva)} \
       options2 {set tlist $tab(OptDir)} \
       view1 {set tlist $tab(GraData)} \
       view2 {set tlist $tab(GraMesh)} \
       view3 {set tlist $tab(GraDiva)}
     }
     set i [lsearch $tlist [focus]]
     set i [expr $i+$depl]
     if {$depl == 1 } {
      if {$i >= [llength $tlist]} {
       set i 0
      }
     } elseif { $i < 0 } { 
      set i [expr [llength $tlist] -1]
     } 
     focus [lindex $tlist $i]
}


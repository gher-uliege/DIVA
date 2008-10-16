#######################################################
# Repac buttons to the same size
#######################################################

proc SetButtonSizes { frame {pad 0} } {
  set maxl 0
# Query each widget in the frame for its width
  foreach w [pack slaves $frame] {
     set len [string length \
        [lindex [$w config -text] 4]]
     if {$len > $maxl} {
        set maxl $len
     }
  }
# Set all the widths to be the same
  foreach w [pack slaves $frame] {
    $w config -width $maxl
  }
# Repack the widgets so they expand into any extra
# space and fill that space with their display.
# eval is used because pack slaves returns a list.
  eval pack [pack slaves $frame] -expand true -fill both -padx $pad
}

#######################################################
# Create Menu Button
#######################################################

proc MenuButton { w name text cmd } {
    button $w.$name -text $text -command $cmd -padx 5
}

#######################################################
# Deactivate Menu Buttons
#######################################################

proc MenuDeact {} {
  foreach w [pack slaves .menu] {
  if { [string compare $w .menu.quit] && [string compare $w .menu.help]\
       && [string compare $w .menu.blank] } {
        $w configure -state disabled
  }
  }
}

#######################################################
# Make Numbered Frames
#######################################################

proc MakeFrame { w n {rel raised} {bord 2}} {
  for {set i 1} {$i <= $n} {incr i 1} {
    frame $w.$i -relief $rel -bd $bord
    pack $w.$i -side top -anchor nw -fill x
  }
}

#######################################################
# ButtonColor Procedure
#######################################################

proc QuitMenu { button { menu {}}} {

  destroy $button$menu
#  .menu$button configure -fg black
}


#######################################################
# Position a dialog box at a reasonable place on the screen.
#######################################################

proc dpos {w {offx 0} {offy 0} } {
    set posx [expr "[winfo rootx .]+$offx"]
    set posy [expr "[winfo rooty .]+$offy"]
    
    wm geometry $w +$posx+$posy
#   wm geometry $w +300+300
}


#######################################################
# Select a depth
#######################################################

proc SelectDepth { dl } {

  upvar $dl depthlist 
  global button Dict
  set button 0

  set w .seldepth
  toplevel $w
  wm title $w "Select depth"
  dpos $w 100 100
  MakeFrame $w 2
  listbox $w.1.list -relief raised -width 10 -height 10 \
    -yscroll "$w.1.scroll set"
  scrollbar $w.1.scroll -relief raised -command "$w.1.list \
    yview"
  pack $w.1.scroll -side right -fill y
  pack $w.1.list -side right -expand true -fill both

  button $w.2.ok -text OK -width 6 -command "set button 1"
  button $w.2.cancel -text $Dict(Cancel) -width 6 -command "set button -1"
  pack $w.2.ok $w.2.cancel -side left -fill x -expand 1 -padx 5 \
    -pady 10

  eval {$w.1.list insert 0} $depthlist
  $w.1.list select from 0

  bind $w.1.list <B1-Motion> {%W select from [%W nearest %y]}
  bind $w.1.list <Shift-1> {%W select from [%W nearest %y]}
  bind $w.1.list <Shift-B1-Motion> {%W select from [%W nearest %y]}

  set oldFocus [focus]
  grab set $w
  focus $w
  tkwait variable button
  if {$button == 1} {
    set out [$w.1.list curselection]
  } else {
    set out -1
  }
  destroy $w
  focus $oldFocus
  return $out

}

###############
# ErrorMessageBox
#  
##############

proc ErrorMessageBox { title text } {

 global Dict
 return [dialog .errormessagebox $title $text error 0 $Dict(Ok) ]

}

###############
# WarningMessageBox
# prompts a warning message 
##############

proc WarningMessageBox { title text } {

 global Dict
 return [dialog .warningmessagebox $title $text warning 0 $Dict(Ok) ]

}

###############
# YesNoMessageBox
# prompts the user to respond yes or no to a given question
##############

proc YesNoMessageBox { title text default} {

 global Dict
 return [dialog .yesnomessagebox $title $text question $default \
                $Dict(Yes) $Dict(No)] 

}

###############
# general DialogBox
##############

proc dialog {w title text bitmap default args} {
        global button

        # 1. Create the top-level window and divide it into top
        # and bottom parts.

        toplevel $w -class Dialog
        DivaCenterWindow $w
        wm title $w $title
        wm iconname $w Dialog
	  wm geometry $w 500x500  
        frame $w.top -relief raised -bd 1
        pack $w.top -side top -fill both
        frame $w.bot -relief raised -bd 1
        pack $w.bot -side bottom -fill both

        # 2. Fill the top part with the bitmap and message.

        message $w.top.msg -width 6i -text $text\
                        -font -Adobe-Times-Medium-R-Normal-*-20-*
        pack $w.top.msg -side right -expand 1 -fill both\
                        -padx 3m -pady 3m
        if {$bitmap != ""} {
                label $w.top.bitmap -bitmap $bitmap
                pack $w.top.bitmap -side left -padx 3m -pady 3m
        }

        # 3. Create a row of buttons at the bottom of the dialog.

        set i 0
        foreach but $args {
                button $w.bot.button$i -text $but -command\
                                "set button $but"
                if {$but == $default} {
                        frame $w.bot.default -relief sunken -bd 1
                        raise $w.bot.button$i
                        pack $w.bot.default -side left -expand 1\
                                        -padx 3m -pady 2m
                        pack $w.bot.button$i -in $w.bot.default\
                                        -side left -padx 2m -pady 2m\
                                        -ipadx 2m -ipady 1m
                } else {
                        pack $w.bot.button$i -side left -expand 1\
                                        -padx 3m -pady 3m -ipadx 2m -ipady 1m
                }
                incr i
        }
     # 4. Set up a binding for <Return>, if there`s a default,
        # set a grab, and claim the focus too.

        if {$default != ""} {
                bind $w <Return> "$w.bot.button$default flash; \
                        set button $default"
        }
        set oldFocus [focus]
        grab set $w
        focus $w

        # 5. Wait for the user to respond, then restore the focus
        # and return the index of the selected button.

        tkwait variable button
        destroy $w
        focus $oldFocus
        return $button
}

####################################################
proc Bindings { } {

   bind Entry <Key-Left>  {EntryMoveCursor %W -1}
   bind Entry <Key-Right> {EntryMoveCursor %W 1}
   bind Entry <Tab> {EntryTabChange %W 1}
   bind Entry <Shift-Tab> {EntryTabChange %W -1}

   bind all <Control-x> p_cut
   bind all <Control-c> p_copy
   bind all <Control-v> p_paste
   bind all <Control-d> p_data1
   bind all <Control-m> p_fem1
   bind all <Control-a> p_diva
   bind all <M1-d> p_view1
   bind all <M1-m> p_view2
   bind all <M1-a> p_view3
   bind all <Control-q> p_quit
}
####################################################
proc DivaCenterWindow {window } {

  set root_width  [ winfo vrootwidth .]
  set root_height [ winfo vrootheight .]
#  set window_height [ winfo height $window]
#  set window_width [ winfo width $window]
  set window_height 350
  set window_width 400
  set posx  [ expr ($root_width-$window_width)/2 ]
  set posy  [ expr ($root_height-$window_height)/2 ]
  wm geometry $window +$posx+$posy
}

####################################################
proc DivaGetWindowGeometry { window } {

 if { [winfo exist $window] } {
  return [winfo geometry $window]
 }
 return ""

}

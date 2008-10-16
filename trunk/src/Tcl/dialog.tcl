###############
# general DialogBox
##############

proc dialog {w title text bitmap default args} {
        global button

        # 1. Create the top-level window and divide it into top
        # and bottom parts.

        toplevel $w -class Dialog
        wm title $w $title
        wm iconname $w Dialog
        wm geometry $w 500x500  # pour avoir une fenetre de taille raisonnable
        frame $w.top -relief raised -bd 1
        pack $w.top -side top -fill both
        frame $w.bot -relief raised -bd 1
        pack $w.bot -side bottom -fill both

        # 2. Fill the top part with the bitmap and message.

        message $w.top.msg -text $text\				
                        -font -Adobe-Times-Medium-R-Normal-*-25-* 	
        pack $w.top.msg -side right -expand 1 -fill both\
                        -padx 1m -pady 1m
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


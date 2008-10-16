#######################################################
# p_copy Procedure
#######################################################

proc p_copy {} {

# puts [focus]
# puts [winfo class [focus]]
# puts copy
 #clipboard append "gggggg"
}

#######################################################
# p_paste Procedure
#######################################################

proc p_paste {} {
 # puts "PASTE"
 #set w [winfo class [focus]]
 #if { $w == "Entry" } {
 #if [catch {selection get} sel] {
 # if [catch {selection get -selection CLIPBOARD} sel] {
 #  puts "no selection"
 #  return}
 #}
 #$w insert 1 "lklklk" 
 #} 
}

#######################################################
# p_cut Procedure
#######################################################

proc p_cut {} {
 # puts cut
 # set w [winfo class [focus]]
 # selection clear $w
 # selection append
}




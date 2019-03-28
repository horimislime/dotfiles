# ctrl-cmd-r to align window to right
defaults write com.crowdcafe.windowmagnet expandWindowEastComboKey '{keyCode=15;modifierFlags=1310720;}'

# ctrl-cmd-l to align window to left
defaults write com.crowdcafe.windowmagnet expandWindowWestComboKey '{keyCode=37;modifierFlags=1310720;}'

# ctrl-cmd-f to make window full screen
defaults write com.crowdcafe.windowmagnet maximizeWindowComboKey '{keyCode=3;modifierFlags=1310720;}'

# Do not snap windows
defaults write com.crowdcafe.windowmagnet useMouseGestures -int 0

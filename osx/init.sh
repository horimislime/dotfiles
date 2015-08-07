#!/bin/bash

if [ ! -e ~/Pictures/screenshots ]
  then
  mkdir ~/Pictures/screenshots
fi

# Screenshot directory
defaults write com.apple.screencapture location ~/Pictures/screenshots

echo "SystemUIServer needs to be restarted. Press [y] to continue."
read key
case ${key} in
  "y" )
  killall SystemUIServer;;
  * )
  echo "Not restarted."
esac

chflags nohidden ~/Library
defaults write com.apple.Finder FXPreferredViewStyle Nlsv
# Show all files
defaults write com.apple.finder AppleShowAllFiles -boolean true

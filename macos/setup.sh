#!/bin/sh

cd $(dirname $0)

defaults import com.crowdcafe.windowmagnet com.crowdcafe.windowmagnet.plist
defaults import net.shinyfrog.bear net.shinyfrog.bear.plist
defaults import com.tapbots.Pastebot2Mac com.tapbots.Pastebot2Mac.plist

# Key repeat speed (needs to sign out)
defaults write -g InitialKeyRepeat -int 10
defaults write -g KeyRepeat -int 1
defaults write com.apple.finder AppleShowAllFiles YES
killall Finder

# Screenshot dir
mkdir  -p $HOME/Pictures/screenshots
defaults write com.apple.screencapture location $HOME/Pictures/screenshots
killall SystemUIServer

# ssh
mkdir -p $HOME/.ssh
ln -fsn $PWD/ssh_config $HOME/.ssh/config

# Karabiner
mkdir -p $HOME/.config/karabiner/assets/complex_modifications/

ln -fsn $PWD/karabiner/karabiner.json $HOME/.config/karabiner/karabiner.json
ln -fsn $PWD/karabiner/assets/complex_modifications/1586223175.json $HOME/.config/karabiner/assets/complex_modifications/1586223175.json
ln -fsn $PWD/karabiner/assets/complex_modifications/1647998283.json $HOME/.config/karabiner/assets/complex_modifications/1647998283.json
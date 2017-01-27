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

defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Dock

## Automatically hide Dock
defaults write com.apple.dock autohide -bool true

## Dock size
defaults write com.apple.dock tilesize -int 54

## Go to screensaver if the cursor is on bottom-left corner
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0

## Show desktop if the cursor is on top-right corner
defaults write com.apple.dock wvous-tr-corner -int 4
defaults write com.apple.dock wvous-tr-modifier -int 0

# Finder

## Open ~/ in new window
defaults write com.apple.finder NewWindowTarget -string "PfDe"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

## Show Path bar
defaults write com.apple.finder ShowPathbar -bool true

## Show all files
defaults write com.apple.finder AppleShowAllFiles -boolean true

## Show ~/Library
chflags nohidden ~/Library

# Trackpad

## Tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Others

## Statusbar's clock format
defaults write com.apple.menuextra.clock 'DateFormat' -string 'MMM d EEE  H:mm:ss'

## Save screenshots as .png
defaults write com.apple.screencapture type -string "png"

## Never go into computer sleep mode
sudo systemsetup -setcomputersleep Off > /dev/null

## Increase sound quality for Bluetooth headphones/headsets
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

## Enable full keyboard access for all controls
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

## Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

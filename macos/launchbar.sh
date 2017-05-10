#!/bin/bash

defaults write at.obdev.LaunchBar ClipboardHistoryCapacity -string "-7"
defaults write at.obdev.LaunchBar ClipboardHistoryPreferPlainText -int 1
defaults write at.obdev.LaunchBar ShowClipboardHistoryHotKey -string "6144@4"

defaults write at.obdev.LaunchBar LaunchBarHotKey -string "6144@37"
defaults write at.obdev.LaunchBar LaunchBarHotKeyEnabled -int 1
defaults write at.obdev.LaunchBar ShowDockIcon -int 0
defaults write at.obdev.LaunchBar SpotlightHotKeyEnabled -int 0
defaults write at.obdev.LaunchBar Theme -string "at.obdev.LaunchBar.theme.ElCapitan"

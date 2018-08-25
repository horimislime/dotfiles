#!/bin/sh

cd $(dirname $0)

mkdir -p $HOME/.config/fish/functions
mkdir -p $HOME/.emacs.d/config
mkdir -p $HOME/.emacs.d/vendor
mkdir -p $HOME/.homebrew/bin
mkdir -p $HOME/.local/bin
mkdir -p $HOME/.tmux.d

$PWD/update-symlink.sh

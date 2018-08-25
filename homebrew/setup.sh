#!/bin/sh

cd $(dirname $0)

git clone git@github.com:Homebrew/brew.git $HOME/.homebrew
export PATH=$HOME/.homebrew/bin:$PATH
export HOMEBREW_CACHE=$HOME/.homebrew/caches

brew doctor
$PWD/update-packages.sh

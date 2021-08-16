#!/bin/bash

git submodule update --init --recursive

# set up dotfiles
dotfiles/setup.sh

# install packages
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    sudo apt-get update
    xargs -a ./linux/pkg-list sudo apt-get install -y
elif [[ "$OSTYPE" == "darwin"* ]]; then
    homebrew/setup.sh
else
    echo "$OSTYPE is not supported."
fi

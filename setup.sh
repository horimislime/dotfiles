#!/bin/bash

git submodule update --init --recursive

# set up dotfiles
dotfiles/setup.sh

# install packages
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    linux/setup.sh
elif [[ "$OSTYPE" == "darwin"* ]]; then
    macos/setup.sh
    homebrew/setup.sh
else
    echo "$OSTYPE is not supported."
fi

#!/bin/bash

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
git submodule update --init --recursive

# set up dotfiles
$SCRIPTPATH/dotfiles/setup.sh

# install packages
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    $SCRIPTPATH/linux/setup.sh
elif [[ "$OSTYPE" == "darwin"* ]]; then
    $SCRIPTPATH/macos/setup.sh
    $SCRIPTPATH/homebrew/setup.sh
else
    echo "$OSTYPE is not supported."
fi

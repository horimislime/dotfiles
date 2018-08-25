#!/bin/sh

cd $(dirname $0)

mkdir -p $HOME/.config/fish/functions
curl -Lo $HOME/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher

#!/usr/bin/env bash

for filename in `git ls-tree --name-only HEAD`; do
    if [ -e $HOME/$filename ]; then
	echo "$filename already exists. Skip."
    else
	ln -Fs $PWD/$filename $HOME/$filename
	echo installed $filename
    fi
done

#!/usr/bin/env bash

for filename in `git ls-tree --name-only HEAD`; do
	ln -fsn $PWD/$filename $HOME/$filename
	echo installed $filename
done

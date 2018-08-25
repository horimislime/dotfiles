#!/usr/bin/env bash

cd $(dirname $0)

for filename in `find . -type f ! -name $(basename $0) ! -name "setup.sh" -printf '%P\n' `; do
	ln -fsn $PWD/$filename $HOME/$filename
	echo installed $filename
done

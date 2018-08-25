#!/bin/sh

cd $(dirname $0)

mkdir -p $HOME/.rbenv/plugins
git clone https://github.com/sstephenson/rbenv-default-gems.git $HOME/.rbenv/plugins/rbenv-default-gems
ln -fsn $PWD/default-gems $HOME/.rbenv/default-gems

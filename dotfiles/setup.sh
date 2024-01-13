#!/bin/sh

cd $(dirname $0)

# Emacs
EMACS_HOME=$HOME/.config/emacs
mkdir -p $EMACS_HOME
ln -fsn $PWD/emacs/init.el $EMACS_HOME/init.el
ln -fsn $PWD/emacs/custom.el $EMACS_HOME/custom.el
ln -fsn $PWD/emacs/config $EMACS_HOME/config

# Git
mkdir -p $HOME/.config/git
ln -fsn $PWD/.gitconfig $HOME/.config/git/config
ln -fsn $PWD/.tigrc $HOME/.tigrc

# Go
mkdir -p $HOME/.go

# tmux
mkdir -p $HOME/.tmux.d

# zsh
mkdir -p $HOME/.zfunctions
ln -fsn $PWD/.zshrc $HOME/.zshrc
ln -fsn $PWD/.zsh.d/theme-pure/pure.zsh $HOME/.zfunctions/prompt_pure_setup
ln -fsn $PWD/.zsh.d/theme-pure/async.zsh $HOME/.zfunctions/async

# etc.
mkdir -p $HOME/.local/bin
ln -fsn $PWD/.textlintrc $HOME/.textlintrc

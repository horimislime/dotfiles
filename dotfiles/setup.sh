#!/bin/sh

cd $(dirname $0)

# Emacs
ln -fsn $PWD/emacs-plus $HOME/.config/emacs-plus

EMACS_HOME=$HOME/.config/emacs
mkdir -p $EMACS_HOME
ln -fsn $PWD/emacs/init.el $EMACS_HOME/init.el
ln -fsn $PWD/emacs/custom.el $EMACS_HOME/custom.el
ln -fsn $PWD/emacs/assets $EMACS_HOME/assets
ln -fsn $PWD/emacs/config $EMACS_HOME/config
ln -fsn $PWD/emacs/packages $EMACS_HOME/packages

# Git
mkdir -p $HOME/.config/git
ln -fsn $PWD/.gitconfig $HOME/.config/git/config
ln -fsn $PWD/.tigrc $HOME/.tigrc

# Go
mkdir -p $HOME/.go

# tmux
mkdir -p $HOME/.tmux.d
ln -fsn $PWD/.tmux.conf $HOME/.tmux.conf

# zsh
mkdir -p $HOME/.zfunctions
ln -fsn $PWD/.zshrc $HOME/.zshrc

# etc.
mkdir -p $HOME/.local/bin
ln -fsn $PWD/.textlintrc $HOME/.textlintrc


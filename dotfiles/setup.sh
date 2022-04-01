#!/bin/sh

cd $(dirname $0)

# Emacs
mkdir -p $HOME/.emacs.d
ln -fsn $PWD/.emacs.d/init.el $HOME/.emacs.d/init.el
ln -fsn $PWD/.emacs.d/custom.el $HOME/.emacs.d/custom.el

# Git
mkdir -p $HOME/.config/git
ln -fsn $PWD/.gitconfig $HOME/.config/git/config
ln -fsn $PWD/.tigrc $HOME/.tigrc

# tmux
mkdir -p $HOME/.tmux.d

# zsh
mkdir -p $HOME/.zfunctions
ln -fsn $PWD/.zshrc $HOME/.zshrc
ln -fsn $PWD/.zsh.d/theme-pure/pure.zsh $HOME/.zfunctions/prompt_pure_setup
ln -fsn $PWD/.zsh.d/theme-pure/async.zsh $HOME/.zfunctions/async

# etc.
mkdir -p $HOME/.homebrew/bin
mkdir -p $HOME/.local/bin
ln -fsn $PWD/.textlintrc $HOME/.textlintrc

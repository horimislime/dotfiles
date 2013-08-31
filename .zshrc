#========================================
# General settings
#========================================
EDITOR=emacs

typeset -U fpath
fpath=(
    $HOME/.zsh.d/*(/N)
    $fpath
)
autoload -U colors; colors
autoload -U $(echo $HOME/.zsh.d/functions/*(:t))

#Command history
HISTFILE=~/.zsh_history
HISTSIZE=6000000
SAVEHIST=6000000

#Discard duplicate history
setopt hist_ignore_all_dups
setopt hist_ignore_dups

setopt inc_append_history
setopt extended_history

#History search
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

#'cd' with directory name
setopt auto_cd
#Records 'cd' history and autocomplete with 'cd -[Tab]'
setopt auto_pushd

#Correct typo
setopt correct

#Disable beep
setopt nolistbeep

setopt list_types
setopt auto_param_keys
setopt mark_dirs

#Prevent exiting session with EOF
setopt ignore_eof

show_buffer_stack() {
  POSTDISPLAY="
stack: $LBUFFER"
  zle push-line-or-edit
}
zle -N show_buffer_stack

#========================================
# Completion
#========================================
#Autocomplete command option
autoload -U compinit
compinit
zstyle ':completion:*:default' menu select=1

#Complete path even after characters(ex.--prefix=/usr...
setopt magic_equal_subst

setopt auto_param_keys
setopt noautoremoveslash

#Ignore upper/lower case in autocomplete
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

#Shift-tab to reverse in directory autocomplete
bindkey "^[[Z" reverse-menu-complete

#z.sh
_Z_CMD=j
source ~/.zsh.d/z/z.sh
precmd() {
    _z --add "${pwd -P}"
}

#========================================
# Functions
#========================================
#function _ssh {
#  compadd `fgrep 'Host ' ~/.ssh/config | awk '{print $2}' | sort`;
#}

#========================================
# zsh-syntax-highlighting
#========================================
if [ -f ~/.zsh.d/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
  source ~/.zsh.d/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# =======================================
# **FOR EMACS** Generating shellenv.el
## create emacs env file
#perl -wle \
#    'do { print qq/(setenv "$_" "$ENV{$_}")/ if exists $ENV{$_} } for @ARGV' \
#    PATH > ~/.emacs.d/shellenv.el

#========================================
# Load setting files
#========================================
source ${HOME}/.zsh.d/appearance.zsh
source ${HOME}/.zsh.d/keybinding.zsh
source ${HOME}/.zsh.d/mysql.zsh
source ${HOME}/.zsh.d/.zshalias
source ${HOME}/.zsh.d/antigen-config.zsh

if [[ -f "$HOME/.zshenv" ]]; then
    source "$HOME/.zshenv"
fi

#========================================
# Plugin setting
#========================================
if [ -d ~/.zsh.d/plugins ]; then
        source ~/.zsh.d/plugins/*
fi

#========================================
# Platform specific settings
#========================================
case "${OSTYPE}" in
darwin*)
    [ -f ${HOME}/.zsh.d/.zsh.osx ] && source ${HOME}/.zsh.d/.zsh.osx
    ;;
linux*)
    [ -f ${HOME}/.zsh.linux ] && source ${HOME}/.zsh.d/.zsh.linux
    ;;
esac

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

if [[ -f "$HOME/.zshenv" ]]; then
    source "$HOME/.zshenv"
fi

if [[ -f "${HOME}/.zsh.d/.zsh.local" ]]; then
    source "${HOME}/.zsh.d/.zsh.local"
fi


if [[ -f "${HOME}/bin/.pvm/pvm.sh" ]]; then
    . /Users/horimislime/bin/.pvm/pvm.sh
fi

if [[ -e "$HOME/.rbenv" ]]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

export PYTHONSTARTUP="$HOME/.pyrc"


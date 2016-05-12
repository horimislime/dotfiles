#========================================
# General settings
#========================================
zmodload zsh/zprof
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
cdpath=(.. ~)
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
compinit -u
zstyle ':completion:*:default' menu select=1

#Complete path even after characters(ex.--prefix=/usr...
setopt magic_equal_subst

setopt auto_param_keys
setopt noautoremoveslash

#Ignore upper/lower case in autocomplete
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

#Shift-tab to reverse in directory autocomplete
bindkey "^[[Z" reverse-menu-complete

#========================================
# Plugin setting
#========================================
if [ -d ~/.zsh.d/plugins ]; then
        source ~/.zsh.d/plugins/*
fi

source ~/.zsh.d/antigen-config.zsh
#source ~/.zsh.d/zaw-config.zsh
source ~/.peco/pecorc

#========================================
# Load setting files
#========================================
source ${HOME}/.zsh.d/keybinding.zsh
source ${HOME}/.zsh.d/zshalias
source ${HOME}/.zsh.d/zshenv

#========================================
# Languages
#========================================
source ${HOME}/.python/environment.sh

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

if [[ -f "${HOME}/.zsh.local" ]]; then
    source "${HOME}/.zsh.local"
fi


#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/horimislime/.gvm/bin/gvm-init.sh" ]] && source "/Users/horimislime/.gvm/bin/gvm-init.sh"


#if type zprof > /dev/null 2>&1; then
#    zprof | less
#fi

autoload -U promptinit && promptinit
prompt pure

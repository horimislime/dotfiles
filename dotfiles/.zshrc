fpath=("$HOME/.zfunctions" $fpath)

autoload -U compinit && compinit
autoload -U promptinit; promptinit
setopt auto_cd
setopt auto_pushd
setopt correct
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_no_store
setopt hist_reduce_blanks
setopt ignoreeof
setopt interactive_comments
setopt no_tify
setopt nobeep
setopt share_history

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Environment variables
PATH=$HOME/bin:$PATH
PATH=/usr/local/bin:$PATH
PATH=$HOME/.homebrew/bin:$PATH
PATH=$HOME/.rbenv/shims:$PATH
PATH=$HOME/.nodenv/shims:$PATH
PATH=$HOME/ghq/github.com/flutter/flutter/bin:$PATH
PATH=$HOME/.pub-cache/bin:$PATH
GOPATH=$HOME/.go
LANG=ja_JP.UTF-8
TERM=xterm-256color
export VISUAL='emacsclient -a "" -t'

# google-cloud-sdk
export CLOUDSDK_PYTHON="/usr/local/opt/python@3.8/libexec/bin/python"
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# aliases
alias cp='cp -i'
alias e='emacsclient -a "" -t'
alias find='gfind' # prefer GNU version over BSD's find
alias grep='grep --color=auto'
alias ll='ls -la'
alias mv='mv -i'
alias o='open'
alias oo='open .'
alias reloadrc='source $HOME/.zshrc'
alias sudo='sudo -E ' # inherit user defined env-vars
alias t='tig status'
alias code='code-insiders'

# pure theme
prompt pure
PURE_GIT_UNTRACKED_DIRTY=0

# fzf
function select-history() {
  BUFFER=$(history -n -r 1 | fzf --no-sort +m --query "$LBUFFER" --prompt="history > ")
  CURSOR=$#BUFFER
}
zle -N select-history
bindkey '^r' select-history

function select-git-branch() {
    if [ ! -d $PWD/.git ]; then
	zle reset-prompt
	return
    fi
    local selected_branch=$(git branch -a --format='%(refname:short)' | fzf --no-sort +m --query "$LBUFFER" --prompt="branches > ")
    if [ -n "$selected_branch" ]; then
	BUFFER="git checkout ${selected_branch}"
	zle accept-line
    fi
    zle reset-prompt
}
zle -N select-git-branch
bindkey '^s' select-git-branch

function select-git-repo() {
    local selected_dir=$(ghq list -p | fzf --no-sort +m --query "$LBUFFER" --prompt="repos > ")
    if [ -n "$selected_dir" ]; then
	BUFFER="cd ${selected_dir}"
	zle accept-line
    fi
    zle reset-prompt
}
zle -N select-git-repo
bindkey '^g' select-git-repo

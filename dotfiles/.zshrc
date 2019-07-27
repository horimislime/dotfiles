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

# Environment variables
PATH=/usr/local/bin:$PATH
PATH=$PATH:$HOME/.homebrew/bin
PATH=$PATH:$HOME/.rbenv/shims

ALTERNATE_EDITOR=''
EDITOR='/usr/local/bin/emacsclient -nw'
GOPATH=$HOME/.go
LANG=ja_JP.UTF-8
TERM=xterm-256color

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# aliases
alias cp='cp -i'
alias e='emacsclient -t'
alias find='gfind' # prefer GNU version over BSD's find
alias grep='grep --color=auto'
alias ll='ls -la'
alias mv='mv -i'
alias o='open'
alias oo='open .'
alias reloadrc='source $HOME/.zshrc'
alias sudo='sudo -E ' # inherit user defined env-vars
alias t='tig status'

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
bindkey '^b' select-git-branch

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

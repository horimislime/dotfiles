fpath=("$HOME/.zfunctions" $fpath)

autoload -U compinit && compinit
autoload -U promptinit; promptinit
setopt auto_cd
setopt auto_pushd
setopt correct
setopt extended_history
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
JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/Contents/Home
PYENV_ROOT=$HOME/.pyenv
PATH=/Applications/Android\ Studio.app/Contents/jre/Contents/Home/bin:$PATH
PATH=$HOME/bin:$PATH
PATH=/usr/local/bin:$PATH
PATH=$HOME/.homebrew/bin:$PATH
PATH=$HOME/.rbenv/shims:$PATH
PATH=$HOME/.nodenv/shims:$PATH
PATH=$HOME/.pyenv/shims:$PATH
PATH=$HOME/ghq/github.com/flutter/flutter/bin:$PATH
PATH=$HOME/.pub-cache/bin:$PATH
GOPATH=$HOME/.go
LANG=ja_JP.UTF-8
TERM=xterm-256color
# Use Python package in gcloud sdk
CLOUDSDK_PYTHON_SITEPACKAGES=1
USE_GKE_GCLOUD_AUTH_PLUGIN=True

if [ "$VSCODE_PID" = "" ] && [ "$TERM_PROGRAM" != "vscode" ]; then
    export VISUAL='emacsclient -a "" -t'
#    printenv >> ~/Desktop/envlist.txt
    #    echo "----" >>  ~/Desktop/envlist.txt
else
    export VISUAL='code --wait'
fi

# google-cloud-sdk
export CLOUDSDK_PYTHON=$(which python3)
if [ -d "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk" ]; then
    source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
    source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"
fi

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# aliases
alias cp='cp -i'
alias e='emacsclient -a "" -t'
alias find='gfind' # prefer GNU version over BSD's find
alias xargs='gxargs'
alias grep='grep --color=auto'
alias ll='ls -la'
alias mv='mv -i'
alias o='open'
alias oo='open .'
alias reloadrc='source $HOME/.zshrc'
alias sed='gsed'
alias sudo='sudo -E ' # inherit user defined env-vars
alias t='tig status'
alias tf='terraform'

# pure theme
prompt pure
prompt_newline='%666v' # single line prompt
PROMPT=" $PROMPT"
zstyle :prompt:pure:path color cyan # tweak for dark background
PURE_GIT_UNTRACKED_DIRTY=0

# fzf
function select-history() {
  BUFFER=$(history -n -r 1 | fzf -e --no-sort)
  CURSOR=$#BUFFER
}
zle -N select-history
bindkey '^r' select-history

function select-git-branch() {
    local selected_branch=$(git branch --format='%(refname:short)' | fzf -e --preview "git log {}")
    if [ -n "$selected_branch" ]; then
	BUFFER="git checkout ${selected_branch}"
	zle accept-line
   fi
   zle reset-prompt
}
zle -N select-git-branch
bindkey '^t' select-git-branch

function select-git-repo() {
    local selected_dir=$(ghq list |fzf -e --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/README.*")
    if [ -n "$selected_dir" ]; then
	BUFFER="cd $HOME/ghq/${selected_dir}"
	zle accept-line
    fi
    zle reset-prompt
}
zle -N select-git-repo
bindkey '^g' select-git-repo

# nodenv
if command -v nodenv &> /dev/null; then
    eval "$(nodenv init -)"
fi

# nvm
# if command -v nvm &> /dev/null; then
#     autoload -U add-zsh-hook
#     load-nvmrc() {
# 	local node_version="$(nvm version)"
# 	local nvmrc_path="$(nvm_find_nvmrc)"

# 	if [ -n "$nvmrc_path" ]; then
# 	    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

# 	    if [ "$nvmrc_node_version" = "N/A" ]; then
# 		nvm install
# 	    elif [ "$nvmrc_node_version" != "$node_version" ]; then
# 		nvm use
# 	    fi
# 	elif [ "$node_version" != "$(nvm version default)" ]; then
# 	    echo "Reverting to nvm default version"
# 	    nvm use default
# 	fi
#     }
#     add-zsh-hook chpwd load-nvmrc
#     load-nvmrc
# fi

# Use ssh-agent of 1password for Mac
if [[ "$OSTYPE" == "darwin"* ]]; then
    export SSH_AUTH_SOCK=~/Library/Group\ Containers/2BUA8C4S2C.com.1password/t/agent.sock
fi

# pyenv
if command -v pyenv &> /dev/null; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 500 # history to store for cdr
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

source $HOME/.zsh.d/zaw/zaw.zsh
zstyle ':filter-select:highlight' selected fg=black,bg=white,standout
zstyle ':filter-select' case-insensitive yes

bindkey '^x^d' zaw-cdr
bindkey '^x^r' zaw-history
bindkey '^X^F' zaw-git-files
bindkey '^x^b' zaw-git-branches
bindkey '^x^p' zaw-process
bindkey '^x^t' zaw-tmux

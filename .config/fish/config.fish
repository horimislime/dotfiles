set -x EDITOR '/usr/local/bin/emacsclient -nw'
set -x LANG ja_JP.UTF-8
set -x GOPATH $HOME/.go
set -x TERM xterm-256color

set -x PATH /usr/local/share/git-core/contrib/diff-highlight/ $PATH

alias cp 'cp -i'
alias e 'emacsclient -t'
alias find 'gfind' # prefer GNU find
alias grep 'grep --color=auto'
alias ll 'ls -l'
alias mv 'mv -i'
alias o 'open'
alias oo 'open .'
alias reload-fish 'source $HOME/.config/fish/config.fish'
alias sudo 'sudo -E ' # to inherit user defined env-vars
alias t 'tig status'
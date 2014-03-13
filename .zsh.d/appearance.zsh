#========================================
#Appearance settings
#========================================

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

#VCS settings
autoload -Uz add-zsh-hook
autoload -Uz colors
colors

#Set tab title to working directory
precmd() {
  echo -ne "\033]0;${PWD}\007"
  psvar=()
  LANG=en_US.UTF-8 vcs_info
  [ -n "$vcs_info_msg_0_" ] && psvar[1]="$vcs_info_msg_0_"
}

#Appearance
colors=(
    $'%{\e[0;31m%}' # red
    $'%{\e[0;32m%}' # green
    $'%{\e[0;33m%}' # brown
    $'%{\e[0;34m%}' # blue
    $'%{\e[0;35m%}' # purple
    $'%{\e[0;36m%}' # cyan
    # $'%{\e[0;37m%}' # gray

    ## light colors
    $'%{\e[1;31m%}' # red
    $'%{\e[1;32m%}' # green
    $'%{\e[1;33m%}' # brown
    $'%{\e[1;34m%}' # blue
    $'%{\e[1;35m%}' # purple
    $'%{\e[1;36m%}' # cyan
    # $'%{\e[1;37m%}' # gray
)
colored_user=$colors[$((`echo "$USER" | sum | cut -f1 -d' '`%${#colors}))+1]$USER
colored_host=$colors[$((`echo "$HOST" | sum | cut -f1 -d' '`%${#colors}))+1]"%m"

#Colorize std error message
function redrev() {
    perl -pe 's/^/\e[41m/ && s/$/\e[m/';
}
alias -g RED='2> >(redrev)'

#Colorize autocomplete candidates of 'ls'
#if which perl >/dev/null 2>&1 ;then
#    LS_COLORS=$(echo $LS_COLORS | perl -pe 's/(?<= [=;] ) 01 (?= [;:] )/00/xg')
#fi

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
#zstyle ':completion:*:default' list-colors ${LS_COLORS}


# vcs_info
autoload vcs_info
# only active under git repo
zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:git:*" check-for-changes true
zstyle ":vcs_info:git:*" stagedstr " ✚"
zstyle ":vcs_info:git:*" unstagedstr " ✑ "
zstyle ":vcs_info:git:*" formats " (%b%c%u)"
zstyle ":vcs_info:git:*" actionformats "%b|%a"

PROMPT="%B%F{246}%F{red}%1v%{${reset_color}%}%F{yellow}%2v%{${reset_color}%} ✘%f%b "
PROMPT2="%B%F{246}%_>%f%b "
SPROMPT="Did you mean %B%F{001}%r%f%b? [n,y,a,e]: "

RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"

# man highlight
man() {
        env \
                LESS_TERMCAP_mb=$(printf "\e[1;31m") \
                LESS_TERMCAP_md=$(printf "\e[1;31m") \
                LESS_TERMCAP_me=$(printf "\e[0m") \
                LESS_TERMCAP_se=$(printf "\e[0m") \
                LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
                LESS_TERMCAP_ue=$(printf "\e[0m") \
                LESS_TERMCAP_us=$(printf "\e[1;32m") \
                man "$@"
}
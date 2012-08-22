#========================================
#Appearance settings
#========================================
#VCS settings
autoload -Uz add-zsh-hook
autoload -Uz colors
colors
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

autoload -Uz is-at-least
if is-at-least 4.3.10; then
  zstyle ':vcs_info:git:*' check-for-changes true
  zstyle ':vcs_info:git:*' stagedstr "+"
  zstyle ':vcs_info:git:*' unstagedstr "-"
  zstyle ':vcs_info:git:*' formats '(%s)-[%b] %c%u'
  zstyle ':vcs_info:git:*' actionformats '(%s)-[%b|%a] %c%u'
fi

function _update_vcs_info_msg() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd _update_vcs_info_msg
RPROMPT="%1(v|%F{green}%1v%f|)"
#############


#Set tab title to working directory
precmd() {
echo -ne "\033]0;${PWD}\007"
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


BASE_PROMPT="$colored_user"$'%{\e[1;39m%}'"@$colored_host"$'%{\e[1;39m%}'
case ${UID} in
0)
    PROMPT=$BASE_PROMPT" # "
    PROMPT2="%B%{[32m%}%_#%{[m%}%b "
    SPROMPT="%B%{[32m%}%r is correct? [n,y,a,e]:%{[m%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
    PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
    ;;
*)
    PROMPT=$BASE_PROMPT" %% "
    RPROMPT="%{[32m%}%~%{[m%} "
    PROMPT2="%{[32m%}%_%{[m%} %%"
    SPROMPT="%{[32m%}%r is correct? [n,y,a,e]:%{[m%} "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
    PROMPT="%{[31m%}(SSH->${HOST%%.*})${PROMPT}"
    ;;
esac 
#PROMPT="$colored_user"

#Colorize std error message
function redrev() {
    perl -pe 's/^/\e[41m/ && s/$/\e[m/';
}
alias -g RED='2> >(redrev)'

#Colorize autocomplete candidates of 'ls'
#if which perl >/dev/null 2>&1 ;then
#    LS_COLORS=$(echo $LS_COLORS | perl -pe 's/(?<= [=;] ) 01 (?= [;:] )/00/xg')
#fi

export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

alias ls="ls -G"
alias gls="gls --color"

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
#zstyle ':completion:*:default' list-colors ${LS_COLORS}

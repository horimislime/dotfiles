function __ghq_crtl_c -d 'Checkout branch'
    if not test -e  .git
        return 0
    end

    git branch -a --format='%(refname:short)' | fzf | read select
    if test -n "$select"
        echo
        git checkout $select
    end
    
    commandline -f repaint
end

function fish_user_key_bindings
    ### fzf ###
    bind \cr '__fzf_reverse_isearch'
    bind \eo '__fzf_cd'
    bind \eO '__fzf_cd --hidden'
    if bind -M insert >/dev/null 2>/dev/null
       bind -M insert \cr '__fzf_reverse_isearch'
       bind -M insert \eo '__fzf_cd'
       bind -M insert \eO '__fzf_cd --hidden'
    end

    if set -q FZF_COMPLETE
        bind \t '__fzf_complete'
    end
    ### fzf ###
    ### ghq ###
    bind \cg '__ghq_crtl_g'
    if bind -M insert >/dev/null ^/dev/null
        bind -M insert \cg '__ghq_crtl_g'
    end
    ### ghq ###

    bind \cc '__ghq_crtl_c'
    if bind -M insert >/dev/null ^/dev/null
        bind -M insert \cc '__ghq_crtl_c'
    end
end

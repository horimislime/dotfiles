#Module initialization
cd .zsh.d/autojump
./install.sh --local --destdir $HOME/dotfiles/bin
cd -

#Create aliases
for filename in `git ls-tree --name-only HEAD`; do
    echo $filename
    ln -Fs $HOME/dotfiles/$filename $HOME/$filename
#    if "$0" -eq 0;then
#	echo finish
#   fi
done

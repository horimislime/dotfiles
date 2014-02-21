for filename in `git ls-tree --name-only HEAD`; do
    ln -Fs $PWD/$filename $HOME/$filename
    echo installed $filename
done

ln -Fs $HOME/Dropbox/Sync/emacs.d $HOME/.emacs.d
git submodule init
git submodule update
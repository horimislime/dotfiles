for filename in `git ls-tree --name-only HEAD`; do
    echo $filename
    ln -Fs ./$filename $HOME/$filename
done

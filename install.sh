for filename in `git ls-tree --name-only HEAD`; do
    if [ -e $HOME/$filename ]; then
	echo "$filename already exists. Skip."
    else
	ln -Fs $PWD/$filename $HOME/$filename
	echo installed $filename
    fi
done

git submodule init
git submodule update

# install gvm
curl -s get.gvmtool.net | bash

# install alcatraz
curl -fsSL https://raw.github.com/supermarin/Alcatraz/master/Scripts/install.sh | sh

# go
go get github.com/motemen/hub-pr

git clone https://github.com/sstephenson/rbenv-default-gems.git ~/.rbenv/plugins/rbenv-default-gems

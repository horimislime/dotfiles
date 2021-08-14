git submodule update --init --recursive

# set up dotfiles
dotfiles/setup.sh

# install packages
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    xargs -a ./linux/pkg-list sudo apt-get install
elif [[ "$OSTYPE" == "darwin"* ]]; then
    homebrew/setup.sh
else
    echo "$OSTYPE is not supported."
fi

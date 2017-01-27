#!/bin/sh

if [ ! -e ~/.ssh/id_rsa.pub ]; then
	echo "No ssh public key found. Generating..."
	ssh-keygen -f ~/.ssh/id_rsa -t rsa -N ''
	echo "Done."
fi

cat ~/.ssh/id_rsa.pub |pbcopy
echo "SSH public key copied. Register it to Github and press enter."
read key

if ! which brew >/dev/null; then
	echo "Installing homebrew..."
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update

if brew doctor >/dev/null; then
	echo "homebrew is ready."
else
	echo "Problem exist on homebrew. Check it after installation by running 'brew doctor'."
fi

# golang installation
mkdir ~/.go
export PATH=$PATH:~/.go/bin

echo "Finished🍺"

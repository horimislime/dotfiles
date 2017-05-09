#!/bin/sh

# GitHub
if [ ! -e ~/.ssh/id_rsa.pub ]; then
	echo "No ssh public key found. Generating..."
	ssh-keygen -f ~/.ssh/id_rsa -t rsa -N ''
	echo "Done."

	cat ~/.ssh/id_rsa.pub |pbcopy
  echo "Public key copied. Register it to GitHub and press enter."
  read key
fi

mkdir -p ~/.go/bin
mkdir -p ~/.rbenv/plugins

git clone https://github.com/sstephenson/rbenv-default-gems.git ~/.rbenv/plugins/rbenv-default-gems
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor

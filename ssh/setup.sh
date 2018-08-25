#!/bin/sh

cd $(dirname $0)

if [ ! -e $HOME/.ssh/id_rsa_github.pub ]; then
	echo "Generating key for GitHub..."
	ssh-keygen -f $HOME/.ssh/id_rsa_github -t rsa -N ''
	echo "Done."
fi

if [ ! -e $HOME/.ssh/id_rsa_gitlab.pub ]; then
	echo "Generating key for GitLab..."
	ssh-keygen -f $HOME/.ssh/id_rsa_gitlab -t rsa -N ''
	echo "Done."
fi

cp config_template $HOME/.ssh/config

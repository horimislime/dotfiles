all: bootstrap update install link

bootstrap:
	./etc/bootstrap.sh
	echo "🔧 Bootstrap completed."

link:
	./etc/symlink.sh
	echo "🔌 Symlink created."

install:
	./macos/init.sh
	brew bundle
	pip install -r ./etc/requirements.txt
	echo "📦 Packages and settings are successfuly installed."

update:
	git pull origin master
	git submodule init
	git submodule update
	git submodule foreach git pull origin master
	echo "✨ Everything is up to date."

setup:
	./dotfiles/setup.sh
	./fish/setup.sh
	./homebrew/setup.sh
	./macos/setup.sh
	./python/setup.sh
	./ruby/setup.sh
	./ssh/setup.sh
	@echo 🔧 Bootstrap completed.

update:
	./dotfiles/update-symlink.sh
	./homebrew/update-packages.sh
	@echo ✨ Everything is up to date.

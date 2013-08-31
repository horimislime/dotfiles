source ~/.zsh.d/antigen/antigen.zsh

antigen-use oh-my-zsh

antigen-bundles <<EOBUNDLES
git
github
brew
vagrant
knife
cp
gem
python
zsh-users/zsh-syntax-highlighting
tetsujin/zsh-function-mysql
EOBUNDLES

antigen-theme kennethreitz

antigen-apply
source ~/.zsh.d/antigen/antigen.zsh

antigen use oh-my-zsh

antigen-bundles <<EOBUNDLES
git
brew
vagrant
cp
python
zsh-users/zsh-syntax-highlighting
EOBUNDLES

antigen theme cloud

antigen-apply
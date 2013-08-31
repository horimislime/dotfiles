source ~/.zsh.d/antigen/antigen.zsh

antigen-use oh-my-zsh

antigen-bundles <<EOBUNDLES
git
github
brew
vagrant
knife
EOBUNDLES

antigen-theme robbyrussell

antigen-apply
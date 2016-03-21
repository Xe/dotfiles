#!/bin/bash

set -x

# setlink sets a symlink to my dotfiles repo for the correct file.
function setlink
{
	ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

rm ~/.zshrc
rm ~/.emacs
rm -rf ~/.emacs.d

# set links
setlink .zshrc
setlink .zsh
setlink .gitconfig
setlink .tmux.conf
setlink .spacemacs
setlink .vimrc
setlink .vim

export GOPATH=/home/xena/go
export PATH=/usr/local/go/bin:$PATH

# Golang stuff
((mkdir -p ~/go/{pkg,bin,src})
 go get -u github.com/mattn/todo
 go get -u github.com/motemen/ghq
 go get -u github.com/Xe/tools/license/...
 go get -u github.com/nsf/gocode
 go get -u github.com/rogpeppe/godef
 go get -u golang.org/x/tools/cmd/oracle) &

# Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/

(emacs --daemon
 # Just in case
 sleep 2
 /usr/bin/emacsclient --eval "(kill-emacs)") &

# Vim
(git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
 head -n 52 ~/.vimrc >> ~/.vimrc-temp
 vim -u ~/.vimrc-temp +PluginInstall +qall
 rm ~/.vimrc-temp
 (cd ~/.vim/bundle/YouCompleteMe; ./install.sh --clang-completer) &
 (cd ~/.vim/bundle/vimproc.vim; make) &
 vim +GoInstallBinaries +qall) &

# Fish
(curl -sL get.fisherman.sh | fish
 fish -c "fisher install bass"
 fish -c "fisher install scorphish"
 rm -rf ~/.config/fisherman/conf.d
 ln -s $HOME/code/dotfiles/fish $HOME/.config/fisherman/conf.d) &

wait

echo "Set up!"

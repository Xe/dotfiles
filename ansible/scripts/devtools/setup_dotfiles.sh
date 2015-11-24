#!/bin/bash

set -x

# Sanity check / setup
cd /home/xena
mkdir code ||:

# Clone dotiles
git clone https://github.com/Xe/dotfiles code/dotfiles ||:

# setlink sets a symlink to my dotfiles repo for the correct file.
function setlink
{
        rm -rf $HOME/$1
        ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

# set links
setlink .zshrc
setlink .zsh
setlink .vim
setlink .vimrc
setlink .gitconfig
setlink .tmux.conf
setlink .spacemacs

export GOPATH=/home/xena/go
export PATH=/usr/local/go/bin:$PATH

# Golang stuff
(mkdir -p ~/go/{pkg,bin,src})
go get github.com/mattn/todo
go get github.com/motemen/ghq
go get github.com/Xe/tools/license
go get github.com/nsf/gocode
go get github.com/rogpeppe/godef
go get golang.org/x/tools/cmd/oracle
go get golang.org/x/tools/cmd/goimports

# Set up emacs
emacs --daemon
sleep 2
killall emacs

# More violence
emacs --daemon
sleep 2
killall emacs

echo "Set up!"

#!/bin/bash -x

# Sanity check / setup
cd /home/xena
mkdir code ||:

# Clone dotiles
git clone https://github.com/Xe/dotfiles code/dotfiles ||:

# setlink sets a symlink to my dotfiles repo for the correct file.
function setlink
{
        ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

# set links
setlink .zshrc
setlink .zsh
setlink .gitconfig
setlink .tmux.conf
setlink .spacemacs

export GOPATH=/home/xena/go
export PATH=/usr/local/go/bin:$PATH

# Golang stuff
(mkdir -p ~/go/{pkg,bin,src})
go get -u -v github.com/mattn/todo
go get -u -v github.com/motemen/ghq
go get -u -v github.com/Xe/tools/license/...
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/oracle

# Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/

emacs --daemon
# Just in case
sleep 2
/usr/bin/emacsclient --eval "(kill-emacs)"

echo "Set up!"

#!/bin/bash -x

# setlink sets a symlink to my dotfiles repo for the correct file.
function setlink
{
        ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

rm ~/.zshrc
rm ~/.emacs

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
go get -u github.com/mattn/todo
go get -u github.com/motemen/ghq
go get -u github.com/Xe/tools/license/...
go get -u github.com/nsf/gocode
go get -u github.com/rogpeppe/godef
go get -u golang.org/x/tools/cmd/oracle

# Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d/

emacs --daemon 2>/dev/null
# Just in case
sleep 2
/usr/bin/emacsclient --eval "(kill-emacs)"

emacs --daemon 2>/dev/null
# Just in case
sleep 2
/usr/bin/emacsclient --eval "(kill-emacs)"


echo "Set up!"

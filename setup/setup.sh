#!/bin/bash

set -e

source "$(basename $0)/setup.lib"

mkdir /tmp/xena-install

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
parinstall bin

export GOPATH=/home/xena/go
export PATH=/usr/local/go/bin:$PATH
export TERM=screen

# Spacemacs
parinstall emacs

# Vim
#parinstall vim

# Golang stuff
parinstall golang

# Fish
parinstall fish

# Nim
parinstall nim

# Haskell
#parinstall haskell

wait

echo "Set up!"

echo "Cleaning up log files (gzip)"

tar czf /tmp/xena.log.tgz /tmp/xena-install/* /tmp/emacs.log
rm -rf /tmp/xena-install /tmp/emacs.log

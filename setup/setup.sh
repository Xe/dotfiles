#!/bin/sh

set -e

source "/opt/xena/setup.lib"

mkdir /tmp/xena-install

rm -rf ~/.emacs
rm -rf ~/.emacs.d

# set links
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
touch /tmp/emacs.log
parinstall emacs

# Golang stuff
parinstall golang

# Fish
parinstall fish

echo "installing..."

wait

echo "Set up!"

echo "Cleaning up log files (gzip)"

touch /tmp/xena-install/logs
tar czf /tmp/xena.log.tgz /tmp/xena-install/* /tmp/emacs.log
rm -rf /tmp/xena-install /tmp/emacs.log

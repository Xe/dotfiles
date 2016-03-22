#!/bin/bash

set -e

mkdir /tmp/xena-install

# setlink sets a symlink to my dotfiles repo for the correct file.
function setlink
{
	echo "Linking $1"
	ln -s $HOME/code/dotfiles/$1 $HOME/$1
}

function goget
{
	go get -v -u $1
}

function installgolang
{
	mkdir -p ~/go/{pkg,bin,src}
	goget github.com/mattn/todo
	goget github.com/motemen/ghq
	goget github.com/Xe/tools/license/...
	goget github.com/nsf/gocode
	goget github.com/rogpeppe/godef
	goget golang.org/x/tools/cmd/oracle
	goget golang.org/x/tools/cmd/gorename
	goget github.com/alecthomas/gometalinter
	goget github.com/golang/lint/golint
	goget github.com/kisielk/errcheck
	goget github.com/jstemmer/gotags
	goget github.com/klauspost/asmfmt/cmd/asmfmt
	goget github.com/fatih/motion

	echo 'Done installing go!'
}

function installemacs
{
	git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d/

	echo "Installing emacs packages... (this will take a while)"
	echo "for detailed log output, run:"
	echo "    $ tail -f /tmp/emacs.log"
	emacs --daemon 2> /tmp/emacs.log
	# Just in case
	sleep 2
	echo "killing emacs"
	pkill -9 emacs
	sleep 2
	echo "killing helper processes"
	pkill git
	pkill gpg-agent

	echo 'Done installing emacs!'
}

function installvimplugin
{
	plugin="$1"
	path="$(echo $plugin | cut -d/ -f2)"

	cd ~/.vim/bundle
	git clone https://github.com/$plugin

	cd ~/.vim/bundle/$path
	git submodule update --init --recursive
}

function installycm
{
	cd ~/.vim/bundle/YouCompleteMe
	./install.sh --clang-completer
}

function installvimproc
{
	cd ~/.vim/bundle/vimproc.vim
	make
}

function installvim
{
	(
		echo "Setting up vundle"
		git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim

		cd ~/.vim/bundle
		echo "extracting package list from vimrc..."
		for plugin in $(cat ~/.vimrc | grep Plugin | cut -d"'" -f2)
		do
			parinstall vimplugin $plugin
		done

		wait

		parinstall ycm
		parinstall vimproc
	)
}

function installfish
{
	echo "Setting up fish"
	git clone https://github.com/fisherman/fisherman ~/.local/share/fisherman 2> /dev/null
	mkdir -p ~/.config/fish{,erman}

	echo "Fish config setup"
	ln -s $HOME/code/dotfiles/config.fish $HOME/.config/fish/config.fish

	echo "Fisherman parts"
	ln -s $HOME/code/dotfiles/fish $HOME/.config/fisherman/conf.d

	echo "Installing fisherman plugins"
	fish -l -c "fisher update"
	fish -l -c "fisher install bass"
	fish -l -c "fisher install scorphish"
}

function installbin
{
	setlink bin
}

# Basically a macro to parallely call an install$foo function
function parinstall
{
	(
		echo "installing $1 $2"
		min="$(echo $2 | cut -d/ -f2)"
		install$1 $2 > /tmp/xena-install/$1-$min-install.log 2> /tmp/xena-install/$1-$min-install.err
		echo "installed $1 $2"
	) &
}

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
parinstall vim

# Golang stuff
parinstall golang

# Fish
parinstall fish

wait

echo "Set up!"

echo "Cleaning up log files (gzip)"

tar czf /tmp/xena.log.tgz /tmp/xena-install/* /tmp/emacs.log
rm -rf /tmp/xena-install /tmp/emacs.log

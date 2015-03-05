#Oh-my-zsh cruft
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
DISABLE_AUTO_TITLE="true"
plugins=(git python github git-extras virtualenv virtualenv-wrapper pip)

# Load sourcefiles
source $ZSH/oh-my-zsh.sh
source $HOME/.profile

export DOKKU_CONTROLLER='dokku.xeserv.us'

# Load extended ZSH aliases and completions
for file in ~/.zsh/*
do
	source $file
done

# My path
export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/xena/bin/

export EDITOR=vim

platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'FreeBSD' ]]; then
   platform='freebsd'
elif [[ "$unamestr" == 'Bitrig' ]]; then
   platform='bitrig'
fi

if [[ $platform == 'bitrig' ]]; then
   unalias ls
fi

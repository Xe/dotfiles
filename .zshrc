#Oh-my-zsh cruft
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="robbyrussell"
DISABLE_AUTO_TITLE="false"
plugins=(git python github git-extras virtualenv virtualenv-wrapper pip)

# Load sourcefiles
source $ZSH/oh-my-zsh.sh
source $HOME/.profile

# Load extended ZSH aliases and completions
for file in ~/.zsh/*
do
	source $file
done

# Syntax highlighting like the fish shell
source $HOME/code/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# My path
export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/xena/bin/

export EDITOR=vim


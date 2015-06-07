# Save the location of the current completion dump file.
if [ -z "$ZSH_COMPDUMP" ]; then
	ZSH_COMPDUMP="${ZDOTDIR:-${HOME}}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"
fi

# Load and run compinit
autoload -U compinit
compinit -i -d "${ZSH_COMPDUMP}"

# Use history
SAVEHIST=15000
HISTFILE=~/.zsh_history
setopt sharehistory
setopt extendedhistory

[[ -n "${key[Up]}"      ]] && bindkey  "${key[Up]}"      history-beginning-search-backward
[[ -n "${key[Down]}"    ]] && bindkey  "${key[Down]}"    history-beginning-search-forward

# Superglobs!
setopt extendedglob
unsetopt caseglob

# Load sourcefiles
source $HOME/.profile

# Detect what platform this is for other scripts
platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
	platform='linux'
elif [[ "$unamestr" == 'Bitrig' ]]; then
	platform='bitrig'
elif [[ "$unamestr" == 'Darwin' ]]; then
	platform='osx'
fi

# report long running command CPU usage
REPORTTIME=60

# Detect what kind of proc we have
proc=`uname -p`

# My path
export PATH=/usr/local/sbin:/usr/local/bin:/bin:/sbin:/usr/sbin:/usr/bin:/home/xena/bin:/home/xena/.linuxbrew/bin

# Vim is love, vim is life
export EDITOR=vim

NAME="%n@"

if [ -n "$DOCKER" ]
then
	NAME="$NAME""docker:%m "
else
	NAME="$NAME""%m "
fi

if [[ platform != "linux" ]]
then
	NAME="$NAME""($platform) "
fi

function __ret_status {
	echo "%(?:%{$fg[green]%}➜ :%{$fg[red]%}➜ %s)"
}

PROMPT='$NAME%{$fg_bold[green]%}${PWD/#$HOME/~}%{$reset_color%}
%{$fg_bold[gray]%}$(__ret_status) %{$reset_color%}'

# Load extended ZSH aliases and completions
for file in ~/.zsh/*.zsh
do
	source $file
done

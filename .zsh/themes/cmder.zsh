function collapse_pwd {
	echo $(pwd | sed -e "s,^$HOME,~,")
}

function if_machine {
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	echo "$(hostname) "
fi
}

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

function ret_status {
	echo "%(?:%{$fg[green]%}➜ :%{$fg[red]%}➜ %s)"
}

PROMPT='$NAME%{$fg_bold[green]%}${PWD/#$HOME/~}%{$reset_color%}
%{$fg_bold[gray]%}$(ret_status) %{$reset_color%}'
RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"

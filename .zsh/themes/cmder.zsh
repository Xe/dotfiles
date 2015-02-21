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

function ret_status {
	echo "%(?:%{$fg[green]%}➜ :%{$fg[red]%}➜ %s)"
}

PROMPT='$NAME%{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}
%{$fg_bold[gray]%}$(ret_status) %{$reset_color%}'

RPROMPT='$(git_prompt_info)'" [%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=" "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

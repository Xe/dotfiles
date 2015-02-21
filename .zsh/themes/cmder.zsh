# Based on the Robby Russel zsh theme, except better suited for my needs

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

PROMPT='$NAME%{$fg_bold[green]%}$(collapse_pwd)%{$reset_color%}
%{$fg_bold[gray]%}λ %{$reset_color%}'

RPROMPT='$(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[red]%}branch:"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[red]%} %{$fg[red]%}unstaged changes%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[red]%} %{$fg[green]%}all is clear%{$reset_color%}"

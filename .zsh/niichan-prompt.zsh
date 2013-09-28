# Based on the Robby Russel zsh theme, except better suited for my needs

sname=""

if [[ $(hostname) -eq "rarity" ]]
then
        sname="$fg_bold[white]rarity"
else if [[ $(hostname) -eq "rainbowdash" ]]
then
        sname="$fg_bold[blue]rainbowdash"
else if [[ $(hostname) -eq "sparkle.yolo-swag.com" ]]
then
        sname="$fg_bold[magenta]sparkle"
else if [[ $(hostname) -eq "genesect" ]]
then
        sname="$fg_bold[green]genesect"
else
        sname=$(hostname)
fi
fi
fi
fi

PROMPT='$sname %{$fg_bold[magenta]%}%c$(git_prompt_info) %{$fg_bold[green]%}>%{$fg_bold[magenta]%}%{$fg_bold[magenta]%} % %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=" ($fg_bold[blue]git:%{$fg_bold[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[magenta]%}) %{$fg[yellow]%}X%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[magenta]%})"

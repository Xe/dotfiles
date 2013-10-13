# Like the prompt from Plan 9's RC but with git integration

PROMPT='$(git_prompt_info); %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}"

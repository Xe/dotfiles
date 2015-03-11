# Tab completion from both ends.
setopt completeinword

# Tab completion should be case-insensitive.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
autoload select-word-style
select-word-style shell

# Menu!
zstyle ':completion:*' menu select

# Fuzzy matching
zstyle ':completion:::::' completer _complete _approximate
# limited to 3 errors per character typed
zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

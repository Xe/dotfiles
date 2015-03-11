# Tab completion from both ends.
setopt completeinword

# Tab completion should be case-insensitive.
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
autoload select-word-style
select-word-style shell

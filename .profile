#PS1="[\u@\h] \w> "

alias ltmux="if tmux has; then tmux attach; else tmux new; fi"
alias memusage="ps -a -o comm,rss,vsz | grep $1"

#PATH=$PATH:/home/xena/bin/

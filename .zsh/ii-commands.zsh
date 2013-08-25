#!/bin/zsh

#Aliases that are useful when used with ii

alias iicmd="echo $* >> in"
alias iiroot="cd ~/irc"
alias iijoin="iicmd /j $*"
#alias iiread="cat $1/out"
#alias iitail="iiread $1 | tail -n 10"
alias iipart="iicmd /l $*"
alias iisay="iicmd"
alias iiaway="iicmd /a $*"
alias iitopic="iicmd /t $*"
alias iinick="iicmd /n $1"


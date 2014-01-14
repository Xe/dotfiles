# Aliases for mpd

MPD_REMOTE_HOST="sparkle.yolo-swag.com"

alias play="mpc play > /dev/null"
alias stop="mpc stop > /dev/null"
alias cursong="mpc | head -n 1"
alias next="mpc next -h $MPD_REMOTE_HOST > /dev/null; sleep 1 ; cursong"
alias prev="mpc prev -h $MPD_REMOTE_HOST > /dev/null; sleep 1 ; cursong"
alias np="mpc status"
alias replay="stop; play"


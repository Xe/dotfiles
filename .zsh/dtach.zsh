alias wm="dtach -A /tmp/dvtm -r winch dvtm"
alias session="dtach -A /tmp/dvtm.$1 -r winch /usr/local/bin/dvtm"

function detach {
if [ -S /tmp/$1.dtach ]; then
	dtach -a /tmp/$1.dtach -r winch
else
	dtach -c /tmp/$1.dtach -r winch $1
fi
}

alias wm="dtach -A /tmp/dvtm -r winch dvtm"

function session {
	dtach -A /tmp/$1.dvtm -r winch dvtm
}

function detach {
if [ -S /tmp/$1.dtach ]; then
	dtach -a /tmp/$1.dtach -r winch
else
	dtach -c /tmp/$1.dtach -r winch $1
fi
}

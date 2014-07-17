function dokku {
	if [[ "$1" -eq "create" ]] then
		git remote add dokku dokku@dokku.xeserv.us:${PWD##*/}
		git push dokku master
	else
		ssh dokku@dokku.xeserv.us $*
	fi
}

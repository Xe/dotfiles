function dokku {
	if [ "$1" = "create" ]
	then
		if git remote add dokku dokku@dokku.xeserv.us:${PWD##*/}
		then
			echo "-----> Dokku remote added at xeserv.us"
		else
			echo "!      Dokku remote not added! Do you already have a dokku remote?"
			return
		fi
		git push dokku master
	else
		ssh dokku@dokku.xeserv.us $*
	fi
}

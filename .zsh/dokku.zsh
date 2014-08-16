function dokku {
	if [ "$1" = "create" ]
	then
		appname=$(echo "print(elfs.GenName())" | lua -l elfs)
		if git remote add dokku dokku@dokku.xeserv.us:$appname
		then
			echo "-----> Dokku remote added at xeserv.us"
			echo "-----> Application name is $appname"
		else
			echo "!      Dokku remote not added! Do you already have a dokku remote?"
			return
		fi
		git push dokku master
	else
		ssh dokku@dokku.xeserv.us $*
	fi
}

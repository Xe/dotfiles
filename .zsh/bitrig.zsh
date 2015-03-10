if [[ $platform == 'bitrig' ]]; then
	export LC_CTYPE="en_US.UTF-8"
	if [ -z "ZSH" ]; then
		unalias ls
	fi
fi

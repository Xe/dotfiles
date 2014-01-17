function notes {
	if [ $# -ne 1 ] ; then
		echo "Usage: $0 classname"
		return
	fi

	# Markdown so human.vim kicks in
	vim notes-$1-$(date +%Y-%m-%d).markdown
}

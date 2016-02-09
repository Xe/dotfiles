function notes --argument-names 'filename'
	if test -z "$filename"
		echo "usage: notes <filename>"
		return
	else
		e "$filename-"(date +%Y-%m-%d.markdown)
	end
end

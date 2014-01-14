#cheating uclibc to do UTF-8
if [ -x /sbin/apk ] ; then
	export LC_ALL='en_US.UTF-8'
	export LANG='en_US.UTF-8'
	export LC_CTYPE=C
	export CHARSET=UTF-8
fi

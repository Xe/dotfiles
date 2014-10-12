#Because fuck you my terminals do support 256 colors

case $TERM in
	*xte*)
		export TERM=xterm-256color
		;;
	*scree*)
		export TERM=screen-256color
		;;
	*rxvt*)
		export TERM=rxvt-unicode-256color
esac


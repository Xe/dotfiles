switch $TERM
	case '*xte*'
		set -gx TERM xterm-256color
	case '*scree*'
		set -gx TERM screen-256color
	case '*rxvt*'
		set -gx TERM rxvt-unicode-256color
end

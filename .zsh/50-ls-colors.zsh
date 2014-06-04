# 
# Default prompt and appearance

# Prompting Options
setopt \
	prompt_cr \
	prompt_subst \



# Color module, should give us access to associative array of colors:
# ${color[red]}
autoload colors;
colors;


# -----------------------------------------------------------------------------
# Xterm console colors 
# -----------------------------------------------------------------------------
# Color definitions for ls and complist module
# 	Syntax: all *.avi files appear in bold purple -> *.avi=01;35 
# 	color Codes 
#	0   = default colour
#	1   = bold
#	4   = underlined
#	5   = flashing text
#	7   = reverse field
#	30  = black
#	31  = red
#	32  = green
#	33  = yellow
#	34  = blue
#	35  = purple
#	36  = cyan
#	37  = white
#	38  = light grey
#	40  = black background
#	41  = red background
#	42  = green background
#	43  = orange background
#	44  = blue background
#	45  = purple background
#	46  = cyan background
#	47  = grey background
#	90  = dark grey
#	91  = light red
#	92  = light green
#	93  = yellow
#	94  = light blue
#	95  = light purple
#	96  = turquoise
#	100 = dark grey background
#	101 = light red background
#	102 = light green background
#	103 = yellow background
#	104 = light blue background
#	105 = light purple background
#	106 = turquoise background
#
IMG_COLORS="*.png=00;35:*.jpg=00;35:*.gif=00;35:*.bmp=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.pdf=00;35"
SOUND_COLORS='*.mp3=00;35:*.mod=00;35:*.wav=00;35:*.mid=00;35:*.xm=00;35:*.s3m=00;35'
MOVIE_COLORS='*.mpg=00;35:*.avi=00;35:*.mov=00;35'
ARCHIVE_COLORS='*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.bz2=00;31:*.rpm=00;31:*.deb=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31'
EXE_COLORS='*.cmd=00;32:*.exe=00;32:*.com=00;32:*.btm=00;32:*.bat=00;32'
CODE_COLORS='*.c=00;33:*.h=00;95:*.gl=00;33:*.cc=00;33:*.cpp=00;33:*.ino=00;33:*.m=00;33:*.php=00;33:*.tex=00;33'

# Understood by GNU ls and used for completion
LS_COLORS="no=00:fi=00:di=00;36:ln=00;35:pi=47;33:so=00;35:bd=47;33;00:cd=47;33;00:or=47;31;00:ex=00;32:${IMG_COLORS}:${SOUND_COLORS}:${MOVIE_COLORS}:${ARCHIVE_COLORS}:${EXE_COLORS}:${CODE_COLORS}";


# Understood by BSD ls
CLICOLOR=''
LSCOLORS="gxfxcxdxbxegedabagacad"

LS_OPTIONS_GNU=(-F --color -T 0);
LS_OPTIONS_BSD=(-F -G);

# Set ls colors to completion engine
# ZLS_COLORS is overwritten by the list-colors style
ZLS_COLORS=${LS_COLORS}

# Need to export all these guys 
export LS_COLORS LSCOLORS ZLS_COLORS


# Find the best ls and set the options
case `uname` in
	FreeBSD)
		 MYLS=`which gls`
		 if [ $MYLS != "gls not found" ]; then
		  	alias ls='gls $LS_OPTIONS_GNU'
		  else
		  	alias ls='ls $LS_OPTIONS_BSD'
		  fi
	;;
	NetBSD)
		 MYLS=`which gls`
		 if [ $MYLS != "gls not found" ]; then
			  alias ls='gls $LS_OPTIONS_GNU'
		  else
		  	alias ls='ls $LS_OPTIONS_BSD'
		 fi
	;;
	Darwin)
		if [[ -f '/opt/local/bin/gls' ]]; then
			alias ls='/opt/local/bin/gls $LS_OPTIONS_GNU'
		else
			alias ls='ls $LS_OPTIONS_BSD'
			echo " - FIXME: For better color management, install coreutils package"
		fi
	;;
	Linux)
		alias ls='ls $LS_OPTIONS_GNU'
	;;
	*)
		alias ls='ls $LS_OPTIONS_BSD'
	;;
esac

alias l='ls -lh'
alias la='ls -A'
alias ll='ls -lhA'
alias lsd='ls -ld *(-/DN)'
alias lsa='ls -ld .*'


zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

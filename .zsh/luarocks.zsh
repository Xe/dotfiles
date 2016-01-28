export PATH=$PATH:$HOME/.luarocks/bin
alias lri='luarocks install $* --local'

function mri {
	for package in "$*";
	do
		moonrocks install $package --local
	done
}

eval "$(luarocks path)"

function pythonz_install {
	curl -kL https://raw.github.com/saghul/pythonz/master/pythonz-install | bash
}
[[ -s $HOME/.pythonz/etc/bashrc ]] && source $HOME/.pythonz/etc/bashrc

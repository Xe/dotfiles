alias ssh='ssh -A'

function get-agent {
	eval $(ssh-agent)
}

if [ -z "$SSH_AUTH_SOCK" ]; then
	get-agent > /dev/null
	ssh-add ~/.ssh/id_rsa > /dev/null
fi

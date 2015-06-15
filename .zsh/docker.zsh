alias dockercleancontainers="docker ps -a -notrunc| grep 'Exit' | awk '{print \$1}' | xargs -L 1 -r docker rm"
alias dockercleanimages="docker images -a -notrunc | grep none | awk '{print \$3}' | xargs -L 1 -r docker rmi"
alias dockerclean="dockercleancontainers && dockercleanimages"

function anonubuntu {
	docker run --rm -it ubuntu:trusty /bin/bash
}

function anonirssi {
	docker run --rm -ite TERM=$TERM irssi
}

function psqlprompt {
	docker run -it --link "$1":postgres --rm postgres sh -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres'
}

if [ "$(uname)" = "Darwin" ]; then
	export DOCKER_HOST=tcp://192.168.59.103:2375
fi

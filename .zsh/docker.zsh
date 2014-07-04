alias dockercleancontainers="docker ps -a -notrunc| grep 'Exit' | awk '{print \$1}' | xargs -L 1 -r docker rm"
alias dockercleanimages="docker images -a -notrunc | grep none | awk '{print \$3}' | xargs -L 1 -r docker rmi"
alias dockerclean="dockercleancontainers && dockercleanimages"

function anonubuntu {
	docker run --rm -it ubuntu:trusty /bin/bash
}

function godevel {
	docker run --rm -it -v /home/xena/go:/home/xena/go xena/base-devel /bin/zsh
}

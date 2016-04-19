alias dockercleancontainers "docker ps -a --no-trunc| grep 'Exit' | awk '{print \$1}' | xargs -L 1 -r docker rm"
alias dockercleanimages "docker images -a --no-trunc | grep none | awk '{print \$3}' | xargs -L 1 -r docker rmi"
alias dockerclean "dockercleancontainers; and dockercleanimages"

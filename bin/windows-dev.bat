@ECHO OFF

docker run -d --name shell -v /c/Users/Christine/Go/src:/home/xena/go/src -v /dev/log:/dev/log -v /c/Users/Christine/.bbdb:/home/xena/.bbdb -v /c/Users/Christine:/home/host -v /var/run/docker.sock --restart=always -ite TERM=cygwin xena/dotfiles /bin/zsh -l

echo "$ docker attach shell"

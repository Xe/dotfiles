#!/bin/sh
mkdir shell
chown 1000:1000 shell

docker run \
	-dit \
	-v /home/rancher/shell:/home/xena/var \
	-v /home/rancher/shell/bin:/home/xena/bin \
	-v /var/run/docker.sock \
	-v /lib/modules/3.19.3-rancher/kernel:/lib/modules/3.19.3-rancher/kernel:ro \
	--net=host \
	--privileged \
	--restart=always \
	--name shell \
	xena/dotfiles

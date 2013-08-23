#!/bin/sh

if [ -x /sbin/apk ]
then
	echo "Alpine Linux detected"
	sudo apk update
	sudo apk upgrade
	exit
fi

if [ -x /usr/bin/apt-get ]
then
	echo "Debuntu detected"
	sudo apt-get update -y
	sudo apt-get upgrade -y
	sudo apt-get dist-upgrade -y
	exit
fi

#!/bin/sh

#Check for alpine linux
if [ -x /sbin/apk ]
then
	sudo apk add bash wget # BusyBox wget is inadequate 
fi

wget --no-check-certificate https://raw.github.com/tokland/arch-bootstrap/master/arch-bootstrap.sh 
mkdir $1
sudo bash arch-bootstrap.sh -a x86_64 $1

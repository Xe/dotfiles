wget http://dl-3.alpinelinux.org/alpine/v2.5/main/x86_64/apk-tools-static-2.3.4-r0.apk

tar -xzf apk-tools-static-2.3.4-r0.apk

mkdir $1
sudo ./sbin/apk.static -X http://repos.lax-noc.com/alpine/edge/main -U --allow-untrusted --root $1 --initdb add alpine-base alpine-sdk
mkdir -p ./$1/proc

sudo mknod -m 666 ./$1/dev/full c 1 7
sudo mknod -m 666 ./$1/dev/ptmx c 5 2
sudo mknod -m 644 ./$1/dev/random c 1 8
sudo mknod -m 644 ./$1/dev/urandom c 1 9
sudo mknod -m 666 ./$1/dev/zero c 1 5
sudo mknod -m 666 ./$1/dev/tty c 5 0

sudo rm -f ./$1/dev/null && sudo mknod -m 666 ./$1/dev/null c 1 3

sudo cp /etc/resolv.conf ./$1/etc/
mkdir -p ./$1/root

sudo mkdir -p ./$1/etc/apk
echo "http://repos.lax-noc.com/alpine/edge/main" > ./$1/etc/apk/repositories
echo "http://repos.lax-noc.com/alpine/edge/testing" >> ./$1/etc/apk/repositories

sudo mount --bind /proc ./$1/proc
sudo chroot ./$1 /bin/sh -l

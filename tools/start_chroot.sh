#!/bin/sh -e
user=`whoami`
if [ "$user" != "root" ]; then
  echo "This script needs root access" >&2
  exit 1
fi
if ! [ -d "$1" ]; then
  echo "Usage: $0 <chroot directory>" >&2
  exit 1
fi
if [ x1 = x`sysctl -ne kernel.grsecurity.chroot_deny_chmod` ]; then
  echo "Warning: can't suid/sgid inside chroot" >&2
fi
if [ x1 = x`sysctl -ne kernel.grsecurity.chroot_deny_chroot` ]; then
  echo "Warning: can't chroot inside chroot" >&2
fi
if [ x1 = x`sysctl -ne kernel.grsecurity.chroot_deny_mknod` ]; then
  echo "Warning: can't mknod inside chroot" >&2
fi
if [ x1 = x`sysctl -ne kernel.grsecurity.chroot_deny_mount` ]; then
  echo "Warning: can't mount inside chroot" >&2
fi
cd "$1"
shift
cp -L /etc/resolv.conf ./etc/ || true
mount -t proc proc ./proc
mount -t sysfs sys ./sys
mount -o bind /dev ./dev
# next line is said to be important for pacman's signature check
mount -o bind /dev/pts ./dev/pts
case $1 in
  -l) shift;;
  -l*) one=${1#-l}; shift; set -- -$one "$@";;
esac
chroot . /bin/sh -l "$@"
umount ./dev/pts
umount ./dev ./sys ./proc

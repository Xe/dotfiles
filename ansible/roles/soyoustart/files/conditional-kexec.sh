#!/bin/bash

set +e
set +x

kernel='/boot/bzImage-4.3.3-xxxx-std-ipv6-64'
cmdline='--reuse-cmdline'

if [ ! -f "$kernel" ]
then
	echo "Needed file $kernel not found. Exiting."
	exit 1
fi

if uname -av | grep grs
then
	echo "Doing a kexec"
	kexec $cmdline $kernel
fi

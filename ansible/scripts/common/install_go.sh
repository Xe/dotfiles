#!/bin/bash

set -e
set -x

cd /usr/local
wget https://storage.googleapis.com/golang/go1.5.3.linux-amd64.tar.gz
tar xf go1.5.3.linux-amd64.tar.gz
rm go1.5.3.linux-amd64.tar.gz

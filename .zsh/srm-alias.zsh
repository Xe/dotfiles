#!/bin/zsh

if [ -x /usr/local/bin/srm ]; then
    alias rm="srm"
fi

if [ -x /usr/bin/srm ]; then
    alias rm="srm"
fi

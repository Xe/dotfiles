if [ -d $HOME/prefix/plan9port ] ; then
    export PLAN9=$HOME/prefix/plan9port
    PATH=$PATH:$PLAN9/bin export PATH
fi

if [ -d $HOME/prefix/plan9port ] ; then
    PLAN9=/home/xena/prefix/plan9port
    export PLAN9
    PATH=$PATH:$PLAN9/bin export PATH
fi

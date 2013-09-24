# This is a very hackish way to get a decently clean syntax for looping through
# either $@ (the arguments) or stdin if $# is 0 (there is none)
#
# Please see the other files for how to use this monster.

metaName="$(dirname $0)/__$(basename $0)"
initGo=$(cat $metaName)
loopNow="$initGo; go"

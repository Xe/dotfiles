map () {
  eval $initDocs
  usage "<lambda-function> [<item>...]"
  example "'<--- \$1 --->'" a b c d
  eval $doneDocs

  typeset f="$1"; shift
  map_() {
    echo ${(e)f}
  }
  eval $loopNow map_
}

mapa () {
  eval $initDocs
  usage "<arithmetic-lambda-function> [<item>...]"
  example "'\$1+5'" 1 2 3
  eval $doneDocs

  typeset f="\$[ $1 ]"; shift
  map $f $@
}

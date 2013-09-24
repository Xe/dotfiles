. $(dirname $0)/loop-magic.zsh

filter() {
  eval $initDocs
  usage "<lambda-function> [list...]"
  example "'echo \$1 | grep --silent an'" anna bryan cecilia daniel
  eval $doneDocs

  typeset f="$1"; shift
  filter_() {
    local x=$1
    eval "$f" && print -- "$x"
  }
  eval $loopNow filter_
  return 0
}

filterf() {
  eval $initDocs
  usage "<function> [list...]"
  example "'test 5 -ge'" 3 5 7 1
  eval $doneDocs

  typeset f="$1 \"\$x\""; shift
  filter "$f" "$@"
}

filtera() {
  eval $initDocs
  usage "<arithmetic-function> [list...]"
  example "'\$1%3 == 0'" {1..6}
  eval $doneDocs

  typeset f="(( $1 ))"; shift
  filter "$f" "$@"
}

fold () {
  eval $initDocs
  usage '<lambda-function> <accumulator> [<item>...]'
  example "'echo \$2\$1\$2'" MIDDLE - O o .
  example "'echo \$x\$acc\$x'" MIDDLE - O o .
  eval $doneDocs

  local body=$1
  local acc=$2
  shift 2
  fold_ () {
    local acc=$1
    local x=$2
    eval "${(e)body}"
  }
  helper_ () {
    acc=$(fold_ $acc $1)
  }
  eval $loopNow helper_
  print -- $acc
}

foldf () {
  addition () { echo $(($1 + $2)) }
  eval $initDocs
  usage '<function> <accumulator> [<item>...]'
  example addition 0 {1..5}
  eval $doneDocs

  typeset f="echo \$($1 \$acc \$x)"; shift
  fold "$f" "$@"
}

folda () {
  eval $initDocs
  usage '<arithmetic-lambda-function> <accumulator> [<item>...]'
  example "'\$1+\$2'" 0 {1..5}
  example "'\$acc+\$x'" 0 {1..5}
  eval $doneDocs

  typeset f="echo \$[ $1 ]"; shift
  fold "$f" "$@"
}

param_is_singleton () {
  local par=$1
  [[ $par[1] != '[' ]]
}

param_is_constant_if_at_all () {
  # A parameter that is constant, but can be optional (think [--] in git add)
  local par=$1
  ([[ $par[1] != '<' ]] &&
    [[ $par[1] != '[' ]])
}

docsRecursionLevel=0
metaName="$(dirname $0)/__init-$(basename $0)"
initDocs=$(cat $metaName)
metaName="$(dirname $0)/__done-$(basename $0)"
doneDocs=$(cat $metaName)

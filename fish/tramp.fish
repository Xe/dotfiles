if test $TERM = "dumb"
  function fish_prompt
    echo "\$ "
  end
  exec sh
end
